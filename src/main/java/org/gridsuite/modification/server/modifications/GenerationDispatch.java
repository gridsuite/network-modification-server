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
import com.powsybl.iidm.modification.scalable.ScalingParameters;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import lombok.Builder;
import lombok.Getter;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.FilterService;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
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
    private static final String SUBSTATION = "substation";
    private static final String REGION_CVG = "regionCvg";
    private static final String IS_PLURAL = "isPlural";
    private static final double EPSILON = 0.001;

    private final GenerationDispatchInfos generationDispatchInfos;

    protected FilterService filterService;

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

    private static double computeTotalActiveBatteryTargetP(Component component) {
        Objects.requireNonNull(component);
        return component.getBusStream().flatMap(Bus::getBatteryStream)
                .filter(battery -> battery.getTerminal().isConnected())
                .mapToDouble(Battery::getTargetP)
                .sum();
    }

    private static double computeTotalAmountFixedSupply(Network network, Component component, List<String> generatorsWithFixedSupply, Reporter reporter) {
        double totalAmountFixedSupply = 0.;
        List<Generator> generatorsWithoutSetpointList = new ArrayList<>();
        totalAmountFixedSupply += generatorsWithFixedSupply.stream().map(network::getGenerator)
                .filter(generator -> generator != null && generator.getTerminal().isConnected() &&
                        generator.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() == component.getNum())
                .peek(generator -> {
                    GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
                    if (startupExtension != null && !Double.isNaN(startupExtension.getPlannedActivePowerSetpoint())) {
                        generator.setTargetP(startupExtension.getPlannedActivePowerSetpoint());
                    } else {
                        generator.setTargetP(0.);
                        generatorsWithoutSetpointList.add(generator);

                    }
                })
                .mapToDouble(Generator::getTargetP).sum();
        if (!generatorsWithoutSetpointList.isEmpty()) {
            report(reporter, Integer.toString(component.getNum()), "GeneratorsWithoutPredefinedActivePowerSetpoint",
                    "${numGeneratorsWithoutSetpoint} generator${isPlural} not have a predefined active power set point",
                    Map.of("numGeneratorsWithoutSetpoint", generatorsWithoutSetpointList.size(),
                            IS_PLURAL, generatorsWithoutSetpointList.size() > 1 ? "s do" : " does"), TypedValue.WARN_SEVERITY);
        }

        // Report details for each generator without a predefined setpoint
        generatorsWithoutSetpointList.forEach(generator ->
                report(reporter, Integer.toString(component.getNum()), "MissingPredefinedActivePowerSetpointForGenerator",
                        "The generator ${generatorId} does not have a predefined active power set point",
                        Map.of("generatorId", generator.getId()), TypedValue.TRACE_SEVERITY));
        return totalAmountFixedSupply;
    }

    private static boolean inDifferentSynchronousComponent(HvdcConverterStation<?> station, int componentNum) {
        Bus bus = station.getTerminal().getBusView().getBus();
        return bus != null && bus.getSynchronousComponent().getNum() != componentNum;
    }

    private static double computeHvdcBalance(Component component) {
        AtomicDouble balance = new AtomicDouble(0.);

        component.getBusStream().forEach(bus -> {
            double hdvcFlow = Stream.concat(bus.getLccConverterStationStream(), bus.getVscConverterStationStream())
                .filter(station -> {
                    // Keep only hvdc linking to another synchronous component
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();

                    boolean station2NotInComponent = station1.getId().equals(station.getId()) && inDifferentSynchronousComponent(station2, component.getNum());
                    boolean station1NotInComponent = station2.getId().equals(station.getId()) && inDifferentSynchronousComponent(station1, component.getNum());
                    return station1NotInComponent || station2NotInComponent;
                })
                .mapToDouble(station -> {
                    // compute hvdc flux : import or export
                    HvdcLine hvdcLine = station.getHvdcLine();
                    HvdcConverterStation<?> station1 = hvdcLine.getConverterStation1();
                    HvdcConverterStation<?> station2 = hvdcLine.getConverterStation2();

                    if (station1.getId().equals(station.getId()) &&
                        hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_RECTIFIER_SIDE_2_INVERTER ||
                        station2.getId().equals(station.getId()) &&
                            hvdcLine.getConvertersMode() == HvdcLine.ConvertersMode.SIDE_1_INVERTER_SIDE_2_RECTIFIER) {
                        return -hvdcLine.getActivePowerSetpoint();
                    } else {
                        return hvdcLine.getActivePowerSetpoint();
                    }
                }).sum();
            balance.addAndGet(hdvcFlow);
        });
        return balance.get();
    }

    private static Double getGeneratorMarginalCost(Generator generator) {
        GeneratorStartup startupExtension = generator.getExtension(GeneratorStartup.class);
        if (startupExtension != null && !Double.isNaN(startupExtension.getMarginalCost())) {
            return startupExtension.getMarginalCost();
        }
        return null;
    }

    private static Map<Double, List<String>> getGeneratorsByMarginalCost(List<Generator> generators, Reporter reporter, String reporterSuffixKey) {
        Map<Double, List<String>> generatorsByMarginalCost = new TreeMap<>();

        // set targetP to 0
        generators.forEach(generator -> generator.setTargetP(0.));

        // get generators with marginal cost
        List<Generator> generatorsWithMarginalCost = generators.stream()
                .filter(generator -> getGeneratorMarginalCost(generator) != null)
                .collect(Collectors.toList());
        int nbNoCost = generators.size() - generatorsWithMarginalCost.size();
        if (nbNoCost > 0) {
            report(reporter, reporterSuffixKey, "NbGeneratorsWithNoCost", "${nbNoCost} generator${isPlural} been discarded from generation dispatch because of missing marginal cost. Their active power set point has been set to 0",
                    Map.of("nbNoCost", nbNoCost,
                            IS_PLURAL, nbNoCost > 1 ? "s have" : " has"),
                    TypedValue.INFO_SEVERITY);
        }
        generators.stream()
            .filter(generator -> getGeneratorMarginalCost(generator) == null)
            .forEach(g -> report(reporter, reporterSuffixKey, "MissingMarginalCostForGenerator", "The generator ${generator} does not have a marginal cost",
                    Map.of(GENERATOR, g.getId()), TypedValue.TRACE_SEVERITY)
            );

        // build map of generators by marginal cost
        generatorsWithMarginalCost.sort(Comparator.comparing(GenerationDispatch::getGeneratorMarginalCost));
        generatorsWithMarginalCost.forEach(g -> {
            Double marginalCost = getGeneratorMarginalCost(g);
            generatorsByMarginalCost.computeIfAbsent(marginalCost, k -> new ArrayList<>());
            generatorsByMarginalCost.get(marginalCost).add(g.getId());
        });

        return generatorsByMarginalCost;
    }

    private static void reportUnknownSubstations(Network network, List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrderingInfos, Reporter reporter, String reporterSuffixKey) {
        if (!CollectionUtils.isEmpty(substationsGeneratorsOrderingInfos)) {
            substationsGeneratorsOrderingInfos.forEach(sInfo ->
                    sInfo.getSubstationIds().forEach(sId -> {
                        Substation substation = network.getSubstation(sId);
                        if (substation == null) {
                            report(reporter, reporterSuffixKey, "SubstationNotFound", "Substation ${substation} not found",
                                    Map.of(SUBSTATION, sId), TypedValue.WARN_SEVERITY);
                        }
                    }));
        }
    }

    private static List<Generator> computeAdjustableGenerators(Network network, Component component, List<String> generatorsWithFixedSupply,
                                                               List<SubstationsGeneratorsOrderingInfos> substationsGeneratorsOrderingInfos,
                                                               Reporter reporter) {
        List<String> generatorsToReturn = new ArrayList<>();
        String reporterSuffixKey = Integer.toString(component.getNum());

        // log substations not found
        reportUnknownSubstations(network, substationsGeneratorsOrderingInfos, reporter, reporterSuffixKey);

        // get all connected generators in the component
        List<Generator> generators = component.getBusStream().flatMap(Bus::getGeneratorStream).collect(Collectors.toList());

        // remove generators with fixed supply
        generators.removeIf(generator -> generatorsWithFixedSupply.contains(generator.getId()));

        Map<Double, List<String>> generatorsByMarginalCost = getGeneratorsByMarginalCost(generators, reporter, reporterSuffixKey);
        generatorsByMarginalCost.forEach((mCost, gList) -> {  // loop on generators of same cost
            if (!CollectionUtils.isEmpty(substationsGeneratorsOrderingInfos)) {  // substations hierarchy provided
                // build mapGeneratorsBySubstationsList, that will contain all the generators with the same marginal cost as mCost contained in each list of substations
                LinkedHashMap<Integer, Set<String>> mapGeneratorsBySubstationsList = new LinkedHashMap<>();

                AtomicInteger i = new AtomicInteger(0);
                substationsGeneratorsOrderingInfos.forEach(sInfo -> {
                    mapGeneratorsBySubstationsList.computeIfAbsent(i.get(), k -> new TreeSet<>());

                    // get generators with marginal cost == mCost in all substations of the current list
                    sInfo.getSubstationIds().forEach(sId -> {
                        Substation substation = network.getSubstation(sId);
                        if (substation != null) {
                            substation.getVoltageLevelStream().forEach(v ->
                                v.getGeneratorStream().filter(g -> {
                                    Double generatorCost = getGeneratorMarginalCost(g);
                                    return generatorCost != null && generatorCost.equals(mCost);
                                }).forEach(g -> mapGeneratorsBySubstationsList.get(i.get()).add(g.getId())));
                        }
                    });

                    i.incrementAndGet();
                });

                // loop until all the generators have been encountered
                AtomicBoolean finished = new AtomicBoolean(false);
                while (!finished.get()) {
                    finished.set(true);
                    mapGeneratorsBySubstationsList.values().forEach(generatorsSet -> {
                        if (generatorsSet.isEmpty()) {  // no generators
                            return;
                        }
                        Optional<String> gId = generatorsSet.stream().findFirst();
                        generatorsToReturn.add(gId.get());
                        generatorsSet.remove(gId.get());
                        finished.set(false);
                    });
                }

                // add in the result the generators with same cost not found in mapGeneratorsBySubstationsList sorted in alphabetical order
                gList.stream().sorted().forEach(gId -> {
                    if (!generatorsToReturn.contains(gId)) {
                        generatorsToReturn.add(gId);
                    }
                });
            } else {  // no substations hierarchy provided
                // add in the result the generators in gList sorted in alphabetical order
                gList.stream().sorted().forEach(generatorsToReturn::add);
            }
        });

        if (generatorsToReturn.isEmpty()) {
            report(reporter, reporterSuffixKey, "NoAvailableAdjustableGenerator", "There is no adjustable generator",
                Map.of(), TypedValue.WARN_SEVERITY);
        }

        return generatorsToReturn.stream().map(network::getGenerator).toList();
    }

    private static class GeneratorTargetPListener extends DefaultNetworkListener {
        private final Reporter reporter;
        private final String suffixKey;
        private final List<Generator> updatedGenerators = new ArrayList<>();

        GeneratorTargetPListener(Reporter reporter, String suffixKey) {
            this.reporter = reporter;
            this.suffixKey = suffixKey;
        }

        @Override
        public void onUpdate(Identifiable identifiable, String attribute, String variantId, Object oldValue, Object newValue) {
            if (identifiable.getType() == IdentifiableType.GENERATOR && attribute.equals("targetP") && Double.compare((double) oldValue, (double) newValue) != 0) {
                updatedGenerators.add((Generator) identifiable);
            }
        }

        public void endReport(List<Generator> adjustableGenerators) {
            // report updated generators
            report(reporter, suffixKey, "TotalGeneratorSetTargetP", "The active power set points of ${nbUpdatedGenerator} generator${isPlural} have been updated as a result of generation dispatch",
                    Map.of("nbUpdatedGenerator", updatedGenerators.size(), IS_PLURAL, updatedGenerators.size() > 1 ? "s" : ""), TypedValue.INFO_SEVERITY);
            updatedGenerators.forEach(g -> report(reporter, suffixKey, "GeneratorSetTargetP", "The active power set point of generator ${generator} has been set to ${newValue} MW",
                    Map.of(GENERATOR, g.getId(), "newValue", g.getTargetP()), TypedValue.TRACE_SEVERITY));

            // report unchanged generators
            int nbUnchangedGenerators = adjustableGenerators.size() - updatedGenerators.size();
            if (nbUnchangedGenerators > 0) {
                List<String> updatedGeneratorsIds = updatedGenerators.stream().map(Identifiable::getId).toList();
                report(reporter, suffixKey, "TotalGeneratorUnchangedTargetP", "${nbUnchangedGenerator} eligible generator${isPlural} not been selected by the merit order algorithm. Their active power set point has been set to 0",
                        Map.of("nbUnchangedGenerator", nbUnchangedGenerators,
                                IS_PLURAL, nbUnchangedGenerators > 1 ? "s have" : " has"), TypedValue.INFO_SEVERITY);
                adjustableGenerators.stream()
                        .filter(g -> !updatedGeneratorsIds.contains(g.getId()))
                        .forEach(g -> report(reporter, suffixKey, "GeneratorUnchangedTargetP", "Generator ${generator} has not been selected by the merit order algorithm. Its active power set point has been set to 0",
                                Map.of(GENERATOR, g.getId()), TypedValue.TRACE_SEVERITY));
            }
            // report the max marginal cost used
            Double maxUsedMarginalCost = updatedGenerators.stream()
                    .map(GenerationDispatch::getGeneratorMarginalCost)
                    .filter(Objects::nonNull)
                    .mapToDouble(Double::doubleValue).max().orElseThrow();

            report(reporter, suffixKey, "MaxUsedMarginalCost", "Marginal cost: ${maxUsedMarginalCost}",
                    Map.of("maxUsedMarginalCost", maxUsedMarginalCost), TypedValue.INFO_SEVERITY);
        }
    }

    @Builder
    @Getter
    private static class GeneratorsFrequencyReserve {
        private final List<String> generators;
        private final double frequencyReserve;
    }

    @Override
    public void initApplicationContext(NetworkModificationApplicator modificationApplicator) {
        filterService = modificationApplicator.getFilterService();
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

    private List<String> exportFilters(List<GeneratorsFilterInfos> generatorsFilters, Network network, Reporter subReporter) {
        if (CollectionUtils.isEmpty(generatorsFilters)) {
            return List.of();
        }
        var filters = generatorsFilters.stream().collect(Collectors.toMap(GeneratorsFilterInfos::getId, GeneratorsFilterInfos::getName, (id1, id2) -> id1, LinkedHashMap::new));

        // export filters
        String workingVariantId = network.getVariantManager().getWorkingVariantId();
        UUID uuid = ((NetworkImpl) network).getUuid();
        Map<UUID, FilterEquipments> exportedGenerators = filterService
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
            .distinct()
            .collect(Collectors.toList());
    }

    private List<String> collectGeneratorsWithoutOutage(Network network, Reporter subReporter) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithoutOutage(), network, subReporter);
    }

    private List<String> collectGeneratorsWithFixedSupply(Network network, Reporter subReporter) {
        return exportFilters(generationDispatchInfos.getGeneratorsWithFixedSupply(), network, subReporter);
    }

    private List<GeneratorsFrequencyReserve> collectGeneratorsWithFrequencyReserve(Network network, Reporter subReporter) {
        return generationDispatchInfos.getGeneratorsFrequencyReserve().stream().map(g -> {
            List<String> generators = exportFilters(g.getGeneratorsFilters(), network, subReporter);
            return GeneratorsFrequencyReserve.builder().generators(generators).frequencyReserve(g.getFrequencyReserve()).build();
        }).collect(Collectors.toList());
    }

    private static double computeGenFrequencyReserve(Generator generator,
                                                     List<GeneratorsFrequencyReserve> generatorsFrequencyReserve) {
        AtomicReference<Double> freqReserve = new AtomicReference<>(0.);
        generatorsFrequencyReserve.forEach(g -> {
            if (g.getGenerators().contains(generator.getId())) {
                freqReserve.set(g.getFrequencyReserve());
            }
        });
        return freqReserve.get();
    }

    private double reduceGeneratorMaxPValue(Generator generator,
                                            List<String> generatorsWithoutOutage,
                                            List<GeneratorsFrequencyReserve> generatorsFrequencyReserve) {
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
        double genFrequencyReserve = computeGenFrequencyReserve(generator, generatorsFrequencyReserve);
        return Math.max(generator.getMinP(), res * (1. - genFrequencyReserve / 100.));
    }

    private void reportDisconnectedGenerators(List<Generator> globalDisconnectedGenerators, int componentNum, Reporter reporter) {
        List<Generator> componentDisconnectedGenerators = globalDisconnectedGenerators.stream()
                .filter(g -> g.getTerminal().getBusView() != null && g.getTerminal().getBusView().getConnectableBus() != null &&
                        g.getTerminal().getBusView().getConnectableBus().getSynchronousComponent().getNum() == componentNum)
                .toList();
        if (!componentDisconnectedGenerators.isEmpty()) {
            report(reporter, Integer.toString(componentNum), "TotalDisconnectedGenerator", "${nbDisconnectedGenerator} generator${isPlural} been discarded from generation dispatch because their are disconnected. Their active power set point remains unchanged",
                    Map.of("nbDisconnectedGenerator", componentDisconnectedGenerators.size(),
                            IS_PLURAL, componentDisconnectedGenerators.size() > 1 ? "s have" : " has"),
                    TypedValue.INFO_SEVERITY);
            componentDisconnectedGenerators.forEach(g ->
                report(reporter, Integer.toString(componentNum), "DisconnectedGenerator", "Generator ${generator} has been discarded from generation dispatch because it is disconnected. Its active power set point remains unchanged",
                        Map.of(GENERATOR, g.getId()), TypedValue.TRACE_SEVERITY)
            );
        }
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        Collection<Component> synchronousComponents = network.getBusView().getBusStream()
            .filter(Bus::isInMainConnectedComponent)
            .map(Bus::getSynchronousComponent)
            .collect(collectingAndThen(toCollection(() -> new TreeSet<>(comparingInt(Component::getNum))), ArrayList::new));

        report(subReporter, "", "NbSynchronousComponents", "Network has ${scNumber} synchronous component${isPlural}: ${scList}",
                Map.of("scNumber", synchronousComponents.size(),
                        IS_PLURAL, synchronousComponents.size() > 1 ? "s" : "",
                        "scList", synchronousComponents.stream().map(sc -> "SC" + sc.getNum()).collect(Collectors.joining(", "))),
                TypedValue.INFO_SEVERITY);

        // all disconnected generators at network level (for report purpose)
        List<Generator> disconnectedGenerators = network.getGeneratorStream()
                .filter(g -> !g.getTerminal().isConnected())
                .toList();

        // get generators for which there will be no reduction of maximal power
        List<String> generatorsWithoutOutage = collectGeneratorsWithoutOutage(network, subReporter);

        // get generators with fixed supply
        List<String> generatorsWithFixedSupply = collectGeneratorsWithFixedSupply(network, subReporter);

        // get generators with frequency reserve
        List<GeneratorsFrequencyReserve> generatorsWithFrequencyReserve = collectGeneratorsWithFrequencyReserve(network, subReporter);

        for (Component component : synchronousComponents) {
            int componentNum = component.getNum();

            Reporter componentReporter = subReporter.createSubReporter("Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum, "Network CC0 " + SYNCHRONOUS_COMPONENT + componentNum);

            Reporter powerToDispatchReporter = componentReporter.createSubReporter(POWER_TO_DISPATCH, POWER_TO_DISPATCH);

            // log disconnected generators attached to this synchronous component
            reportDisconnectedGenerators(disconnectedGenerators, componentNum, powerToDispatchReporter);

            // get total value of connected loads in the connected component
            double totalDemand = computeTotalDemand(component, generationDispatchInfos.getLossCoefficient());
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalDemand", "The total demand is : ${totalDemand} MW",
                Map.of("totalDemand", round(totalDemand)), TypedValue.INFO_SEVERITY);

            // get total supply value for generators with fixed supply
            double totalAmountFixedSupply = computeTotalAmountFixedSupply(network, component, generatorsWithFixedSupply, powerToDispatchReporter);
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountFixedSupply", "The total amount of fixed supply is : ${totalAmountFixedSupply} MW",
                Map.of("totalAmountFixedSupply", round(totalAmountFixedSupply)), TypedValue.INFO_SEVERITY);

            // compute hvdc balance to other synchronous components
            double hvdcBalance = computeHvdcBalance(component);
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalOutwardHvdcFlow", "The HVDC balance is : ${hvdcBalance} MW",
                Map.of("hvdcBalance", round(hvdcBalance)), TypedValue.INFO_SEVERITY);

            double activeBatteryTotalTargetP = computeTotalActiveBatteryTargetP(component);
            report(powerToDispatchReporter, Integer.toString(componentNum), "TotalActiveBatteryTargetP", "The battery balance is : ${batteryBalance} MW",
                    Map.of("batteryBalance", round(activeBatteryTotalTargetP)), TypedValue.INFO_SEVERITY);

            double totalAmountSupplyToBeDispatched = totalDemand - totalAmountFixedSupply - hvdcBalance - activeBatteryTotalTargetP;
            if (totalAmountSupplyToBeDispatched < 0.) {
                report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountFixedSupplyExceedsTotalDemand", "The total amount of fixed supply exceeds the total demand",
                    Map.of(), TypedValue.WARN_SEVERITY);
                continue;
            } else {
                report(powerToDispatchReporter, Integer.toString(componentNum), "TotalAmountSupplyToBeDispatched", "The total amount of supply to be dispatched is : ${totalAmountSupplyToBeDispatched} MW",
                    Map.of("totalAmountSupplyToBeDispatched", round(totalAmountSupplyToBeDispatched)), TypedValue.INFO_SEVERITY);
            }

            // get adjustable generators in the component
            List<Generator> adjustableGenerators = computeAdjustableGenerators(network, component, generatorsWithFixedSupply,
                                                                               generationDispatchInfos.getSubstationsGeneratorsOrdering(),
                                                                               powerToDispatchReporter);

            double realized = 0.;
            if (!adjustableGenerators.isEmpty()) {
                // stacking of adjustable generators to ensure the totalAmountSupplyToBeDispatched
                List<Scalable> generatorsScalable = adjustableGenerators.stream().map(generator -> {
                    double minValue = generator.getMinP();
                    double maxValue = reduceGeneratorMaxPValue(generator, generatorsWithoutOutage, generatorsWithFrequencyReserve);
                    return (Scalable) Scalable.onGenerator(generator.getId(), minValue, maxValue);
                }).toList();

                Reporter stackingReporter = componentReporter.createSubReporter(STACKING, STACKING);

                GeneratorTargetPListener listener = new GeneratorTargetPListener(stackingReporter, Integer.toString(componentNum));
                network.addListener(listener);

                Scalable scalable = Scalable.stack(generatorsScalable.toArray(Scalable[]::new));
                realized = scalable.scale(network, totalAmountSupplyToBeDispatched, new ScalingParameters().setAllowsGeneratorOutOfActivePowerLimits(true));

                listener.endReport(adjustableGenerators);
                network.removeListener(listener);
            }

            Reporter resultReporter = componentReporter.createSubReporter(RESULT, RESULT);

            if (Math.abs(totalAmountSupplyToBeDispatched - realized) < EPSILON) {
                Map<String, List<Generator>> generatorsByRegion = getGeneratorsByRegion(network, component);

                report(resultReporter, Integer.toString(componentNum), "SupplyDemandBalanceCouldBeMet", "The supply-demand balance could be met",
                    Map.of(), TypedValue.INFO_SEVERITY);
                generatorsByRegion.forEach((region, generators) -> {
                    Map<EnergySource, Double> activePowerSumByEnergySource = getActivePowerSumByEnergySource(generators);
                    report(resultReporter, Integer.toString(componentNum), "SumGeneratorActivePower" + region, "Sum of generator active power setpoints in ${region} region: ${sum} MW (NUCLEAR: ${nuclearSum} MW, THERMAL: ${thermalSum} MW, HYDRO: ${hydroSum} MW, WIND AND SOLAR: ${windAndSolarSum} MW, OTHER: ${otherSum} MW).",
                            Map.of("region", region,
                                    "sum", activePowerSumByEnergySource.values().stream().reduce(0d, Double::sum),
                                    "nuclearSum", activePowerSumByEnergySource.getOrDefault(EnergySource.NUCLEAR, 0d),
                                    "thermalSum", activePowerSumByEnergySource.getOrDefault(EnergySource.THERMAL, 0d),
                                    "hydroSum", activePowerSumByEnergySource.getOrDefault(EnergySource.HYDRO, 0d),
                                    "windAndSolarSum", activePowerSumByEnergySource.getOrDefault(EnergySource.WIND, 0d) + activePowerSumByEnergySource.getOrDefault(EnergySource.SOLAR, 0d),
                                    "otherSum", activePowerSumByEnergySource.getOrDefault(EnergySource.OTHER, 0d)
                                    ), TypedValue.INFO_SEVERITY);
                });
            } else {
                double remainingPowerImbalance = totalAmountSupplyToBeDispatched - realized;
                report(resultReporter, Integer.toString(componentNum), "SupplyDemandBalanceCouldNotBeMet", "The supply-demand balance could not be met : the remaining power imbalance is ${remainingPower} MW",
                    Map.of("remainingPower", round(remainingPowerImbalance)), TypedValue.WARN_SEVERITY);
            }
        }
    }

    private Map<String, List<Generator>> getGeneratorsByRegion(Network network, Component component) {
        // get all connected generators  that are inside the synchronous component and the substationIds associated.
        List<Generator> connectedGenerators = network.getGeneratorStream()
                .filter(g -> g.getTerminal().isConnected() && g.getTerminal().getBusView().getBus().getSynchronousComponent().getNum() == component.getNum())
                .toList();
        List<String> substationIds = connectedGenerators.stream()
                .map(g -> g.getTerminal().getVoltageLevel().getSubstation().map(Substation::getId).orElse(null))
                .filter(Objects::nonNull)
                .toList();
        // get all substations with "regionCvg" property name
        Map<String, String> substationIdPropertiesMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(substationIds)) {
            substationIds.forEach(sId -> {
                Substation substation = network.getSubstation(sId);
                if (!substation.getPropertyNames().isEmpty() && hasCvgPropertyName(substation.getPropertyNames())) {
                    substation.getPropertyNames().forEach(property -> {
                        if (REGION_CVG.equals(property)) {
                            substationIdPropertiesMap.put(substation.getId(), substation.getProperty(property));
                        }
                    });
                }
            });
        }

        // group substationIds by region
        Map<String, List<String>> groupedSubstationIds = substationIdPropertiesMap.keySet().stream().collect(Collectors.groupingBy(substationIdPropertiesMap::get));

        // iterate over groupedSubstationIds and check for each substation list if it's related to the connected generators
        Map<String, List<Generator>> generatorsByRegion = new HashMap<>();

        groupedSubstationIds.forEach((region, substationList) -> {
            List<Generator> connectedGeneratorsWithSubstation = connectedGenerators.stream()
                    .filter(g -> substationList.contains(g.getTerminal().getVoltageLevel().getSubstation().map(Substation::getId).orElse(null)))
                    .toList();
            generatorsByRegion.put(region, connectedGeneratorsWithSubstation);
        });

        return generatorsByRegion;
    }

    private boolean hasCvgPropertyName(Set<String> propertyNames) {
        return propertyNames.stream().anyMatch(REGION_CVG::equals);
    }

    private Map<EnergySource, Double> getActivePowerSumByEnergySource(List<Generator> generators) {
        return generators.stream().collect(Collectors.toMap(Generator::getEnergySource, Generator::getTargetP, Double::sum));
    }

    private static double round(double value) {
        return Math.round(value * 10) / 10.;
    }
}
