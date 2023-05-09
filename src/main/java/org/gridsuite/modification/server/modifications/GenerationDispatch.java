/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.google.common.util.concurrent.AtomicDouble;
import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.ReportBuilder;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.scalable.Scalable;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Component;
import com.powsybl.iidm.network.DefaultNetworkListener;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.HvdcConverterStation;
import com.powsybl.iidm.network.HvdcLine;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.dto.GeneratorsWithoutOutageInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.context.ApplicationContext;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeSet;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.Comparator.comparingInt;
import static java.util.stream.Collectors.collectingAndThen;
import static java.util.stream.Collectors.toCollection;
import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATION_DISPATCH_ERROR;
import static org.gridsuite.modification.server.modifications.ModificationUtils.distinctByKey;

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
    private final List<String> generatorsWithoutOutage = new ArrayList<>();

    private final Map<Integer, Double> totalDemand = new HashMap<>();
    private final Map<Integer, Double> remainingPowerImbalance = new HashMap<>();
    private final Map<Integer, Double> hvdcBalance = new HashMap<>();

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

    private double computeTotalAmountFixedSupply(Component component) {
        double totalAmountFixedSupply = 0.;
        totalAmountFixedSupply += fixedSupplyGenerators.get(component.getNum()).stream().filter(generator -> generator.getTerminal().isConnected())
            .mapToDouble(Generator::getTargetP).sum();
        return totalAmountFixedSupply;
    }

    private double computeHvdcBalance(Component component) {
        AtomicDouble balance = new AtomicDouble(0.);

        component.getBusStream().forEach(bus -> {
            double hdvcFlow = Stream.concat(bus.getLccConverterStationStream(), bus.getVscConverterStationStream())
                .filter(station -> {
                    // Keep only hvdc linking to another synchronous component
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();
                    boolean station2NotInComponent = station1.getId().equals(station.getId()) &&
                        station2.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() != component.getNum();
                    boolean station1NotInComponent = station2.getId().equals(station.getId()) &&
                        station1.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() != component.getNum();
                    return station1NotInComponent || station2NotInComponent;
                })
                .mapToDouble(station -> {
                    // compute hvdc flux : import or export
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();

                    if ((station1.getId().equals(station.getId()) &&
                        hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER) ||
                        (station2.getId().equals(station.getId()) &&
                            hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER)) {
                        return -1. * hvdcLine.getActivePowerSetpoint();
                    } else {
                        return hvdcLine.getActivePowerSetpoint();
                    }
                }).sum();
            balance.addAndGet(hdvcFlow);
        });
        return balance.get();
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

    public double getTotalDemand(int numSC) {
        return totalDemand.get(numSC);
    }

    public double getRemainigPowerImbalance(int numSC) {
        return remainingPowerImbalance.get(numSC);
    }

    public double getHvdcBalance(int numSC) {
        return hvdcBalance.get(numSC);
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        double lossCoefficient = generationDispatchInfos.getLossCoefficient();
        if (lossCoefficient < 0. || lossCoefficient > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The loss coefficient must be between 0 and 100");
        }
        double defaultOutageRate = generationDispatchInfos.getDefaultOutageRate();
        if (defaultOutageRate < 0. || defaultOutageRate > 100.) {
            throw new NetworkModificationException(GENERATION_DISPATCH_ERROR, "The default outage rate must be between 0 and 100");
        }
    }

    private void collectGeneratorsWithoutOutage(Network network, Reporter subReporter, ApplicationContext context) {
        if (CollectionUtils.isEmpty(generationDispatchInfos.getGeneratorsWithoutOutage())) {
            return;
        }
        var filters = generationDispatchInfos.getGeneratorsWithoutOutage().stream().filter(distinctByKey(GeneratorsWithoutOutageInfos::getId))
            .collect(Collectors.toMap(GeneratorsWithoutOutageInfos::getId, GeneratorsWithoutOutageInfos::getName));

        // export filters
        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        Map<UUID, FilterEquipments> generatorsFilters = context.getBean(FilterService.class)
            .exportFilters(new ArrayList<>(filters.keySet()), uuid, workingVariantId).stream()
            // keep only generators filters
            .filter(filterEquipments -> !CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()) &&
                filterEquipments.getIdentifiableAttributes().stream().allMatch(identifiableAttributes -> identifiableAttributes.getType() == IdentifiableType.GENERATOR))
            .peek(t -> t.setFilterName(filters.get(t.getFilterId())))
            .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));

        // filters with generators not found
        Map<UUID, FilterEquipments> filtersWithGeneratorsNotFound = generatorsFilters.entrySet().stream()
            .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        filtersWithGeneratorsNotFound.values().forEach(f -> {
            var generatorsIds = String.join(", ", f.getNotFoundEquipments());
            report(subReporter, "filterGeneratorsNotFound", "Cannot find the following generators ${generatorsIds} in filter ${filterName}",
                Map.of("generatorIds", generatorsIds, "filterName", filters.get(f.getFilterId())),
                TypedValue.WARN_SEVERITY);
        });

        generatorsWithoutOutage.addAll(generatorsFilters.values()
            .stream()
            .filter(f -> !filtersWithGeneratorsNotFound.containsKey(f.getFilterId()))
            .flatMap(f -> generatorsFilters.get(f.getFilterId()).getIdentifiableAttributes().stream())
            .map(IdentifiableAttributes::getId)
            .collect(Collectors.toList()));
    }

    private double reduceGeneratorMaxPValue(Generator generator) {
        double res = generator.getMaxP();
        if (!generatorsWithoutOutage.contains(generator.getId())) {
            GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
            if (startupExtension != null &&
                !Double.isNaN(startupExtension.getForcedOutageRate()) &&
                !Double.isNaN(startupExtension.getPlannedOutageRate())) {
                res *= (1. - startupExtension.getForcedOutageRate()) * (1. - startupExtension.getPlannedOutageRate());
            } else {
                res *= 1. - generationDispatchInfos.getDefaultOutageRate() / 100.;
            }
        }
        return res;
    }

    @Override
    public void apply(Network network, Reporter subReporter, ApplicationContext context) {
        Collection<Component> synchronousComponents = network.getBusView().getBusStream()
            .filter(Bus::isInMainConnectedComponent)
            .map(Bus::getSynchronousComponent)
            .collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingInt(Component::getNum))), ArrayList::new));

        // get generators for which there will be no reduction of maximal power
        collectGeneratorsWithoutOutage(network, subReporter, context);

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
            double totalAmountFixedSupply = computeTotalAmountFixedSupply(component);
            report(powerToDispatchReporter, "TotalAmountFixedSupply", "The total amount of fixed supply is : ${totalAmountFixedSupply} MW",
                Map.of("totalAmountFixedSupply", totalAmountFixedSupply), TypedValue.INFO_SEVERITY);

            // compute hvdc balance to other synchronous comppnents
            hvdcBalance.put(componentNum, computeHvdcBalance(component));
            report(powerToDispatchReporter, "TotalOutwardHvdcFlow", "The HVDC balance is : ${hvdcBalance} MW",
                Map.of("hvdcBalance", hvdcBalance.get(componentNum)), TypedValue.INFO_SEVERITY);

            double totalAmountSupplyToBeDispatched = totalDemand.get(componentNum) - totalAmountFixedSupply - hvdcBalance.get(componentNum);
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
                List<Scalable> generatorsScalable = adjustableGenerators.get(componentNum).stream().map(generator -> {
                    double minValue = generator.getMinP();
                    double maxValue = reduceGeneratorMaxPValue(generator);
                    return (Scalable) Scalable.onGenerator(generator.getId(), minValue, maxValue);
                }).collect(Collectors.toList());

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
