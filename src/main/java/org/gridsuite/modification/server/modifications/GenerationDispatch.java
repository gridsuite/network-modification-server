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
import org.gridsuite.modification.server.dto.GeneratorsFilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.context.ApplicationContext;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
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

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatch extends AbstractModification {
    private static final String SYNCHRONOUS_COMPONENT = "SC";
    private static final String POWER_TO_DISPATCH = "PowerToDispatch";
    private static final String STACKING = "Stacking";
    private static final String RESULT = "Result";
    private static final String GENERATOR = "generator";
    private static final double EPSILON = 0.001;

    private final GenerationDispatchInfos generationDispatchInfos;

    public GenerationDispatch(GenerationDispatchInfos generationDispatchInfos) {
        this.generationDispatchInfos = generationDispatchInfos;
    }

    private static void report(Reporter reporter, String suffixKey, String key, String defaultMessage, Map<String, Object> values, TypedValue severity) {
        ReportBuilder builder = Report.builder()
            .withKey(key + suffixKey)
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

    private double computeTotalAmountFixedSupply(Network network, Component component, List<String> generatorsWithFixedSupply, Reporter reporter) {
        double totalAmountFixedSupply = 0.;

        totalAmountFixedSupply += generatorsWithFixedSupply.stream().map(network::getGenerator)
            .filter(generator -> generator != null && generator.getTerminal().isConnected() &&
                generator.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() == component.getNum())
            .peek(generator -> {
                GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
                if (startupExtension != null && !Double.isNaN(startupExtension.getPlannedActivePowerSetpoint())) {
                    generator.setTargetP(startupExtension.getPlannedActivePowerSetpoint());
                } else {
                    generator.setTargetP(0.);
                    report(reporter, Integer.toString(component.getNum()), "MissingPredefinedActivePowerSetpointForGenerator", "The generator ${generator} does not have a predefined active power set point",
                        Map.of(GENERATOR, generator.getId()), TypedValue.WARN_SEVERITY);
                }
            })
            .mapToDouble(Generator::getTargetP).sum();
        return totalAmountFixedSupply;
    }

    private static Component getSynchronousComponentFrom(HvdcConverterStation<?> station) {
        return station.getTerminal().getBusView().getBus().getSynchronousComponent();
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

                    boolean station2NotInComponent = station1.getId().equals(station.getId()) && getSynchronousComponentFrom(station2).getNum() != component.getNum();
                    boolean station1NotInComponent = station2.getId().equals(station.getId()) && getSynchronousComponentFrom(station1).getNum() != component.getNum();
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
                        return -hvdcLine.getActivePowerSetpoint();
                    } else {
                        return hvdcLine.getActivePowerSetpoint();
                    }
                }).sum();
            balance.addAndGet(hdvcFlow);
        });
        return balance.get();
    }

    private List<Generator> computeAdjustableGenerators(Component component, List<String> generatorsWithFixedSupply, Reporter reporter) {
        List<Generator> res;

        // get all generators in the component
        List<Generator> generators = component.getBusStream().flatMap(Bus::getGeneratorStream).collect(Collectors.toList());

        // remove generators with fixed supply
        generators.removeIf(generator -> generatorsWithFixedSupply.contains(generator.getId()));

        // set targetP to 0
        generators.forEach(generator -> generator.setTargetP(0.));

        // adjustable generators : generators with marginal cost
        res = generators.stream().filter(generator -> {
            GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
            boolean marginalCostAvailable = startupExtension != null && !Double.isNaN(startupExtension.getMarginalCost());
            if (!marginalCostAvailable) {
                report(reporter, Integer.toString(component.getNum()), "MissingMarginalCostForGenerator", "The generator ${generator} does not have a marginal cost",
                    Map.of(GENERATOR, generator.getId()), TypedValue.WARN_SEVERITY);
            }
            return marginalCostAvailable;
        }).collect(Collectors.toList());

        // sort generators by marginal cost, and then by alphabetic order of id
        res.sort(Comparator.comparing(generator -> ((Generator) generator).getExtension(GeneratorStartup.class).getMarginalCost())
            .thenComparing(generator -> ((Generator) generator).getId()));

        if (res.isEmpty()) {
            report(reporter, Integer.toString(component.getNum()), "NoAvailableAdjustableGenerator", "There is no adjustable generator",
                Map.of(), TypedValue.WARN_SEVERITY);
        }
        return res;
    }

    private static class GeneratorTargetPListener extends DefaultNetworkListener {
        private final Reporter reporter;
        private final String suffixKey;

        GeneratorTargetPListener(Reporter reporter, String suffixKey) {
            this.reporter = reporter;
            this.suffixKey = suffixKey;
        }

        @Override
        public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
            if (identifiable.getType() == IdentifiableType.GENERATOR &&
                attribute.equals("targetP") &&
                Double.compare((double) oldValue, (double) newValue) != 0) {
                report(reporter, suffixKey, "GeneratorSetTargetP", "Generator ${generator} targetP : ${oldValue} MW --> ${newValue} MW",
                    Map.of(GENERATOR, identifiable.getId(), "oldValue", oldValue, "newValue", newValue), TypedValue.INFO_SEVERITY);
            }
        }
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

    private List<String> exportFilters(List<GeneratorsFilterInfos> generatorsFilters,
                                       Network network, Reporter subReporter, ApplicationContext context) {
        if (CollectionUtils.isEmpty(generatorsFilters)) {
            return List.of();
        }
        var filters = generatorsFilters.stream().collect(Collectors.toMap(GeneratorsFilterInfos::getId, GeneratorsFilterInfos::getName, (id1, id2) -> id1, LinkedHashMap::new));

        // export filters
        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        Map<UUID, FilterEquipments> exportedGenerators = context.getBean(FilterService.class)
            .exportFilters(new ArrayList<>(filters.keySet()), uuid, workingVariantId).stream()
            // keep only generators filters
            .filter(filterEquipments -> !CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()) &&
                filterEquipments.getIdentifiableAttributes().stream().allMatch(identifiableAttributes -> identifiableAttributes.getType() == IdentifiableType.GENERATOR))
            .peek(t -> t.setFilterName(filters.get(t.getFilterId())))
            .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));

        // report filters with generators not found
        Map<UUID, FilterEquipments> filtersWithGeneratorsNotFound = exportedGenerators.entrySet().stream()
            .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
        filtersWithGeneratorsNotFound.values().forEach(f -> {
            var generatorsIds = String.join(", ", f.getNotFoundEquipments());
            report(subReporter, "", "filterGeneratorsNotFound", "Cannot find the following generators ${generatorsIds} in filter ${filterName}",
                Map.of("generatorIds", generatorsIds, "filterName", filters.get(f.getFilterId())),
                TypedValue.WARN_SEVERITY);
        });

        return exportedGenerators.values()
            .stream()
            .filter(f -> !filtersWithGeneratorsNotFound.containsKey(f.getFilterId()))
            .flatMap(f -> exportedGenerators.get(f.getFilterId()).getIdentifiableAttributes().stream())
            .map(IdentifiableAttributes::getId)
            .collect(Collectors.toList());
    }

    private List<String> collectGeneratorsWithoutOutage(Network network, Reporter subReporter, ApplicationContext context) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithoutOutage(), network, subReporter, context);
    }

    private List<String> collectGeneratorsWithFixedSupply(Network network, Reporter subReporter, ApplicationContext context) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithFixedSupply(), network, subReporter, context);
    }

    private double reduceGeneratorMaxPValue(Generator generator, List<String> generatorsWithoutOutage) {
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
        List<String> generatorsWithoutOutage = collectGeneratorsWithoutOutage(network, subReporter, context);

        // get generators with fixed supply
        List<String> generatorsWithFixedSupply = collectGeneratorsWithFixedSupply(network, subReporter, context);

        for (Component component : synchronousComponents) {
            int componentNum = component.getNum();

            Reporter componentReporter = subReporter.createSubReporter("Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum, "Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum);

            Reporter powerToDispatchReporter = componentReporter.createSubReporter(POWER_TO_DISPATCH, POWER_TO_DISPATCH);

            // get total value of connected loads in the connected component
            double totalDemand = computeTotalDemand(component, generationDispatchInfos.getLossCoefficient());
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalDemand", "The total demand is : ${totalDemand} MW",
                Map.of("totalDemand", totalDemand), TypedValue.INFO_SEVERITY);

            // get total supply value for generators with fixed supply
            double totalAmountFixedSupply = computeTotalAmountFixedSupply(network, component, generatorsWithFixedSupply, powerToDispatchReporter);
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountFixedSupply", "The total amount of fixed supply is : ${totalAmountFixedSupply} MW",
                Map.of("totalAmountFixedSupply", totalAmountFixedSupply), TypedValue.INFO_SEVERITY);

            // compute hvdc balance to other synchronous components
            double hvdcBalance = computeHvdcBalance(component);
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalOutwardHvdcFlow", "The HVDC balance is : ${hvdcBalance} MW",
                Map.of("hvdcBalance", hvdcBalance), TypedValue.INFO_SEVERITY);

            double totalAmountSupplyToBeDispatched = totalDemand - totalAmountFixedSupply - hvdcBalance;
            if (totalAmountSupplyToBeDispatched < 0.) {
                report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountFixedSupplyExceedsTotalDemand", "The total amount of fixed supply exceeds the total demand",
                    Map.of(), TypedValue.WARN_SEVERITY);
                continue;
            } else {
                report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountSupplyToBeDispatched", "The total amount of supply to be dispatched is : ${totalAmountSupplyToBeDispatched} MW",
                    Map.of("totalAmountSupplyToBeDispatched", totalAmountSupplyToBeDispatched), TypedValue.INFO_SEVERITY);
            }

            // get adjustable generators in the component
            List<Generator> adjustableGenerators = computeAdjustableGenerators(component, generatorsWithFixedSupply, powerToDispatchReporter);

            double realized = 0.;
            if (!adjustableGenerators.isEmpty()) {
                // stacking of adjustable generators to ensure the totalAmountSupplyToBeDispatched
                List<Scalable> generatorsScalable = adjustableGenerators.stream().map(generator -> {
                    double minValue = generator.getMinP();
                    double maxValue = reduceGeneratorMaxPValue(generator, generatorsWithoutOutage);
                    return (Scalable) Scalable.onGenerator(generator.getId(), minValue, maxValue);
                }).collect(Collectors.toList());

                Reporter stackingReporter = componentReporter.createSubReporter(STACKING, STACKING);

                GeneratorTargetPListener listener = new GeneratorTargetPListener(stackingReporter, Integer.toString(componentNum));
                network.addListener(listener);

                Scalable scalable = Scalable.stack(generatorsScalable.toArray(Scalable[]::new));
                realized = scalable.scale(network, totalAmountSupplyToBeDispatched);

                network.removeListener(listener);
            }

            Reporter resultReporter = componentReporter.createSubReporter(RESULT, RESULT);

            if (Math.abs(totalAmountSupplyToBeDispatched - realized) < EPSILON) {
                report(resultReporter, Integer.toString(componentNum), "SupplyDemandBalanceCouldBeMet", "The supply-demand balance could be met",
                    Map.of(), TypedValue.INFO_SEVERITY);
            } else {
                double remainingPowerImbalance = totalAmountSupplyToBeDispatched - realized;
                report(resultReporter, Integer.toString(componentNum), "SupplyDemandBalanceCouldNotBeMet", "The supply-demand balance could not be met : the remaining power imbalance is ${remainingPower} MW",
                    Map.of("remainingPower", remainingPowerImbalance), TypedValue.WARN_SEVERITY);
            }
        }
    }
}
