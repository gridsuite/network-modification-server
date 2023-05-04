/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.ReportBuilder;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Component;
import com.powsybl.iidm.network.DefaultNetworkListener;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeSet;
import java.util.stream.Collectors;

import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATION_DISPATCH_ERROR;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatch extends AbstractModification {
    private static final String SYNCHRONOUS_COMPONENT = "SC";
    private static final String POWER_TO_DISPATCH = "PowerToDispatch";
    private static final String STACKING = "Stacking";
    private static final String RESULT = "Result";
    private static final double EPSILON = 0.001;

    private final Map<Integer, List<Generator>> fixedSupplyGenerators = new HashMap<>();
    private final Map<Integer, List<Generator>> adjustableGenerators = new HashMap<>();

    private final Map<Integer, Double> totalDemand = new HashMap<>();
    private final Map<Integer, Double> remainingPowerImbalance = new HashMap<>();

    private final GenerationDispatchInfos generationDispatchInfos;

    public GenerationDispatch(GenerationDispatchInfos generationDispatchInfos) {
        this.generationDispatchInfos = generationDispatchInfos;
    }

    private static void report(Reporter reporter, String key, String defaultMessage, Map<String, Object> values, TypedValue severity) {
        ReportBuilder builder = Report.builder()
            .withKey(key)
            .withDefaultMessage(defaultMessage)
            .withSeverity(severity);
        for (Map.Entry<String, Object> valueEntry : values.entrySet()) {
            builder.withValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        reporter.report(builder.build());
    }

    private static double computeTotalActiveLoad(Component component) {
        Objects.requireNonNull(component);
        return component.getBusStream().flatMap(Bus::getLoadStream)
            .filter(load -> load.getTerminal().isConnected())
            .mapToDouble(Load::getP0)
            .sum();
    }

    private static double computeTotalDemand(Component component, double lossCoefficient) {
        double totalLoad = computeTotalActiveLoad(component);
        return totalLoad * (1. + lossCoefficient / 100.);
    }

    private double computeTotalAmountFixedSupply(int numCC) {
        double totalAmountFixedSupply = 0.;
        totalAmountFixedSupply += fixedSupplyGenerators.get(numCC).stream().filter(generator -> generator.getTerminal().isConnected())
            .mapToDouble(Generator::getTargetP).sum();
        return totalAmountFixedSupply;
    }

    private void computeAdjustableGenerators(Component component, Reporter reporter) {
        // get all generators in the component
        List<Generator> generators = component.getBusStream().flatMap(Bus::getGeneratorStream).collect(Collectors.toList());

        // remove non adjustable generators (empty list in this first version)
        generators.removeAll(fixedSupplyGenerators.get(component.getNum()));

        // set targetP to 0
        generators.forEach(generator -> generator.setTargetP(0.));

        // adjustable generators : generators with marginal cost
        adjustableGenerators.put(component.getNum(), generators.stream().filter(generator -> {
            GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
            boolean marginalCostAvailable = startupExtension != null && !Double.isNaN(startupExtension.getMarginalCost());
            if (!marginalCostAvailable) {
                report(reporter, "MissingMarginalCostForGenerator", "The generator ${generator} does not have a marginal cost",
                    Map.of("generator", generator.getId()), TypedValue.WARN_SEVERITY);
            }
            return marginalCostAvailable;
        }).collect(Collectors.toList()));

        // sort generators by marginal cost, and then by alphabetic order of id
        adjustableGenerators.get(component.getNum()).sort(Comparator.comparing(generator -> ((Generator) generator).getExtension(GeneratorStartup.class).getMarginalCost())
            .thenComparing(generator -> ((Generator) generator).getId()));

        if (adjustableGenerators.get(component.getNum()).isEmpty()) {
            report(reporter, "NoAvailableAdjustableGenerator", "There is no adjustable generator",
                Map.of(), TypedValue.WARN_SEVERITY);
        }
    }

    private static class GeneratorTargetPListener extends DefaultNetworkListener {
        private final Reporter reporter;

        GeneratorTargetPListener(Reporter reporter) {
            this.reporter = reporter;
        }

        @Override
        public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
            if (identifiable.getType() == IdentifiableType.GENERATOR &&
                attribute.equals("targetP") &&
                Double.compare((double) oldValue, (double) newValue) != 0) {
                report(reporter, "GeneratorSetTargetP", "Generator ${generator} targetP : ${oldValue} MW --> ${newValue} MW",
                    Map.of("generator", identifiable.getId(), "oldValue", oldValue, "newValue", newValue), TypedValue.INFO_SEVERITY);
            }
        }
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        double lossCoefficient = generationDispatchInfos.getLossCoefficient();
        if (lossCoefficient < 0. || lossCoefficient > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The loss coefficient must be between 0 and 100");
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Collection<Component> synchronousComponents = network.getBusView().getBusStream()
            .filter(Bus::isInMainConnectedComponent)
            .map(Bus::getSynchronousComponent)
            .collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingInt(Component::getNum))), ArrayList::new));

        for (Component component : synchronousComponents) {
            int componentNum = component.getNum();

            remainingPowerImbalance.put(componentNum, 0.);
            fixedSupplyGenerators.put(componentNum, new ArrayList<>());  // no fixed supply generators in this first version

            Reporter componentReporter = subReporter.createSubReporter("Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum, "Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum);

            Reporter powerToDispatchReporter = componentReporter.createSubReporter(POWER_TO_DISPATCH, POWER_TO_DISPATCH);

            // get total value of connected loads in the connected component
            totalDemand.put(componentNum, computeTotalDemand(component, generationDispatchInfos.getLossCoefficient()));
            report(powerToDispatchReporter, "TotalDemand", "The total demand is : ${totalDemand} MW",
                Map.of("totalDemand", totalDemand.get(componentNum)), TypedValue.INFO_SEVERITY);

            // get total supply value for non adjustable generators (will be 0. in this first version)
            double totalAmountFixedSupply = computeTotalAmountFixedSupply(componentNum);
            report(powerToDispatchReporter, "TotalAmountFixedSupply", "The total amount of fixed supply is : ${totalAmountFixedSupply} MW",
                Map.of("totalAmountFixedSupply", totalAmountFixedSupply), TypedValue.INFO_SEVERITY);

            double totalAmountSupplyToBeDispatched = totalDemand.get(componentNum) - totalAmountFixedSupply;
            if (totalAmountSupplyToBeDispatched < 0.) {
                report(powerToDispatchReporter, "TotalAmountFixedSupplyExceedsTotalDemand", "The total amount of fixed supply exceeds the total demand",
                    Map.of(), TypedValue.WARN_SEVERITY);
                continue;
            } else {
                report(powerToDispatchReporter, "TotalAmountSupplyToBeDispatched", "The total amount of supply to be dispatched is : ${totalAmountSupplyToBeDispatched} MW",
                    Map.of("totalAmountSupplyToBeDispatched", totalAmountSupplyToBeDispatched), TypedValue.INFO_SEVERITY);
            }

            // get adjustable generators in the component
            computeAdjustableGenerators(component, powerToDispatchReporter);

            double realized = 0.;
            if (!adjustableGenerators.get(componentNum).isEmpty()) {
                // stacking of adjustable generators to ensure the totalAmountSupplyToBeDispatched
                List<Scalable> generatorsScalable = adjustableGenerators.get(componentNum).stream().map(generator ->
                    (Scalable) Scalable.onGenerator(generator.getId(), generator.getMinP(), generator.getMaxP())
                ).collect(Collectors.toList());

                Reporter stackingReporter = componentReporter.createSubReporter(STACKING, STACKING);

                GeneratorTargetPListener listener = new GeneratorTargetPListener(stackingReporter);
                network.addListener(listener);

                Scalable scalable = Scalable.stack(generatorsScalable.toArray(Scalable[]::new));
                realized = scalable.scale(network, totalAmountSupplyToBeDispatched);

                network.removeListener(listener);
            }

            Reporter resultReporter = componentReporter.createSubReporter(RESULT, RESULT);

            if (Math.abs(totalAmountSupplyToBeDispatched - realized) < EPSILON) {
                report(resultReporter, "SupplyDemandBalanceCouldBeMet", "The supply-demand balance could be met",
                    Map.of(), TypedValue.INFO_SEVERITY);
            } else {
                remainingPowerImbalance.put(componentNum, totalAmountSupplyToBeDispatched - realized);
                report(resultReporter, "SupplyDemandBalanceCouldNotBeMet", "The supply-demand balance could not be met : the remaining power imbalance is ${remainingPower} MW",
                    Map.of("remainingPower", remainingPowerImbalance.get(componentNum)), TypedValue.WARN_SEVERITY);
            }
        }
    }
}
