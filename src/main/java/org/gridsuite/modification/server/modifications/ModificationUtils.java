/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.report.ReportConstants;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.ReportNodeAdder;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateCouplingDeviceBuilder;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder;
import com.powsybl.iidm.modification.topology.TopologyModificationUtils;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.FilterService;
import org.jetbrains.annotations.Nullable;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
// TODO transfer to powsybl-core (com.powsybl.iidm.modification)
// TODO remove public qualifier for all methods
public final class ModificationUtils {

    public static final String DISCONNECTOR = "disconnector_";
    public static final String BREAKER = "breaker_";
    public static final String BUS_BAR_SECTION_ID = "busbarSectionId";
    public static final String NO_VALUE = "No value";
    public static final String LIMITS = "Limits";
    public static final String REACTIVE_LIMITS = "Reactive limits";
    private static final String SETPOINTS = "Setpoints";
    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";
    private static final String CONNECTIVITY = "Connectivity";
    public static final String CONNECTION_NAME_FIELD_NAME = "Connection name";
    public static final String CONNECTION_DIRECTION_FIELD_NAME = "Connection direction";
    public static final String CONNECTION_POSITION_FIELD_NAME = "Connection position";
    public static final String CONNECTION_NAME_FIELD_NAME_1 = "Connection name 1";
    public static final String CONNECTION_DIRECTION_FIELD_NAME_1 = "Connection direction 1";
    public static final String CONNECTION_POSITION_FIELD_NAME_1 = "Connection position 1";
    public static final String CONNECTION_NAME_FIELD_NAME_2 = "Connection name 2";
    public static final String CONNECTION_DIRECTION_FIELD_NAME_2 = "Connection direction 2";
    public static final String CONNECTION_POSITION_FIELD_NAME_2 = "Connection position 2";

    private ModificationUtils() {
    }

    public static ModificationUtils getInstance() {
        return new ModificationUtils();
    }

    public Double zeroIfNull(Double d) {
        return d != null ? d : 0.0;
    }

    public static Double nanIfNull(Double d) {
        return d == null ? Double.NaN : d;
    }

    public VoltageLevel getVoltageLevel(Network network, String voltageLevelId) {
        VoltageLevel voltageLevel = network.getVoltageLevel(voltageLevelId);
        if (voltageLevel == null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, voltageLevelId);
        }
        return voltageLevel;
    }

    Line getLine(Network network, String lineId) {
        Line line = network.getLine(lineId);
        if (line == null) {
            throw new NetworkModificationException(LINE_NOT_FOUND, lineId);
        }
        return line;
    }

    Battery getBattery(Network network, String batteryId) {
        Battery battery = network.getBattery(batteryId);
        if (battery == null) {
            throw new NetworkModificationException(BATTERY_NOT_FOUND, "Battery " + batteryId + " does not exist in network");
        }
        return battery;
    }

    Generator getGenerator(Network network, String generatorId) {
        Generator generator = network.getGenerator(generatorId);
        if (generator == null) {
            throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorId + " does not exist in network");
        }
        return generator;
    }

    VscConverterStation getVscConverterStation(Network network, String converterStationId) {
        VscConverterStation vscConverterStation = network.getVscConverterStation(converterStationId);
        if (vscConverterStation == null) {
            throw new NetworkModificationException(VSC_CONVERTER_STATION_NOT_FOUND, "Vsc converter station  " + converterStationId + " does not exist in network");
        }
        return vscConverterStation;
    }

    //get hvdcline
    HvdcLine getHvdcLine(Network network, String hvdcLineId) {
        HvdcLine hvdcLine = network.getHvdcLine(hvdcLineId);
        if (hvdcLine == null) {
            throw new NetworkModificationException(HVDC_LINE_NOT_FOUND, "Hvdc line  " + hvdcLineId + " does not exist in network");
        }
        return hvdcLine;
    }

    public void controlConnectivity(Network network, String voltageLevelId, String busOrBusbarSectionId, Integer connectionPosition) {
        VoltageLevel voltageLevel = getVoltageLevel(network, voltageLevelId);
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            // bus bar section must exist
            controlBus(network, voltageLevel, busOrBusbarSectionId);
            // check if position is free
            Set<Integer> takenFeederPositions = TopologyModificationUtils.getFeederPositions(voltageLevel);
            var position = getPosition(connectionPosition, busOrBusbarSectionId, network, voltageLevel);
            if (takenFeederPositions.contains(position)) {
                throw new NetworkModificationException(CONNECTION_POSITION_ERROR, "PositionOrder '" + position + "' already taken");
            }
        } else {
            // bus breaker must exist
            controlBus(network, voltageLevel, busOrBusbarSectionId);
        }
    }

    public void controlBus(Network network, VoltageLevel voltageLevel, String busOrBusbarSectionId) {
        if (voltageLevel.getTopologyKind() == TopologyKind.BUS_BREAKER) {
            getBusBreakerBus(voltageLevel, busOrBusbarSectionId);
        } else if (network.getBusbarSection(busOrBusbarSectionId) == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busOrBusbarSectionId);
        }
    }

    public void controlBranchCreation(Network network, String voltageLevelId1, String busOrBusbarSectionId1, Integer connectionPosition1,
                                      String voltageLevelId2, String busOrBusbarSectionId2, Integer connectionPosition2) {
        VoltageLevel voltageLevel1 = getVoltageLevel(network, voltageLevelId1);
        VoltageLevel voltageLevel2 = getVoltageLevel(network, voltageLevelId2);
        if (voltageLevel1.getTopologyKind() == TopologyKind.NODE_BREAKER &&
                voltageLevel2.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            controlConnectivity(network, voltageLevelId1,
                    busOrBusbarSectionId1, connectionPosition1);
            controlConnectivity(network, voltageLevelId2,
                    busOrBusbarSectionId2, connectionPosition2);
        } else {
            // bus or mixed mode
            controlBus(network, voltageLevel1, busOrBusbarSectionId1);
            controlBus(network, voltageLevel2, busOrBusbarSectionId2);
        }
    }

    public int getPosition(Integer defaultPosition, String busOrBusbarSectionId, Network network, VoltageLevel voltageLevel) {
        return defaultPosition != null
                ? defaultPosition
                : getPosition(busOrBusbarSectionId, network, voltageLevel);
    }

    public int getPosition(String busOrBusbarSectionId, Network network, VoltageLevel voltageLevel) {
        var position = 0;
        var bbs = network.getBusbarSection(busOrBusbarSectionId);
        if (bbs == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busOrBusbarSectionId);
        }

        var extensionExist = bbs.getExtension(BusbarSectionPosition.class) != null;
        if (!extensionExist) {
            return position;
        }

        if (voltageLevel.getConnectableStream().anyMatch(c -> !(c instanceof BusbarSection))) {
            var rightRange = TopologyModificationUtils.getUnusedOrderPositionsAfter(bbs);
            if (rightRange.isPresent()) {
                position = rightRange.get().getMinimum();
            } else {
                var leftRange = TopologyModificationUtils.getUnusedOrderPositionsBefore(bbs);
                if (leftRange.isPresent()) {
                    position = leftRange.get().getMaximum();
                } else {
                    throw new NetworkModificationException(POSITION_ORDER_ERROR, "no available position");
                }
            }
        }

        return position;
    }

    public Bus getBusBreakerBus(VoltageLevel voltageLevel, String busId) {
        VoltageLevel.BusBreakerView busBreakerView = voltageLevel.getBusBreakerView();
        Bus bus = busBreakerView.getBus(busId);
        if (bus == null) {
            throw new NetworkModificationException(BUS_NOT_FOUND, busId);
        }
        return bus;
    }

    public int createNodeBreakerCellSwitches(VoltageLevel voltageLevel, String busBarSectionId, String equipmentId,
                                             String equipmentName, String sideSuffix) {
        VoltageLevel.NodeBreakerView nodeBreakerView = voltageLevel.getNodeBreakerView();
        BusbarSection busbarSection = nodeBreakerView.getBusbarSection(busBarSectionId);
        if (busbarSection == null) {
            throw new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, busBarSectionId);
        }

        // creating the disconnector
        int newNode = nodeBreakerView.getMaximumNodeIndex();
        String disconnectorId = DISCONNECTOR + equipmentId + sideSuffix;
        String disconnectorName = equipmentName != null ? DISCONNECTOR + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(disconnectorId)
            .setName(disconnectorName)
            .setKind(SwitchKind.DISCONNECTOR)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(busbarSection.getTerminal().getNodeBreakerView().getNode())
            .setNode2(newNode + 1)
            .add();

        // creating the breaker
        String breakerId = BREAKER + equipmentId + sideSuffix;
        String breakerName = equipmentName != null ? BREAKER + equipmentName + sideSuffix : null;
        nodeBreakerView.newSwitch()
            .setId(breakerId)
            .setName(breakerName)
            .setKind(SwitchKind.BREAKER)
            .setRetained(false)
            .setOpen(false)
            .setFictitious(false)
            .setNode1(newNode + 1)
            .setNode2(newNode + 2)
            .add();

        return newNode + 2;
    }

    public void controlNewOrExistingVoltageLevel(VoltageLevelCreationInfos mayNewVL,
                String existingVoltageLevelId, String bbsOrBusId, Network network) {
        if (mayNewVL != null) {
            controlVoltageLevelCreation(mayNewVL, network);
        } else {
            // use existing VL
            VoltageLevel vl = network.getVoltageLevel(existingVoltageLevelId);
            if (vl == null) {
                throw new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, existingVoltageLevelId);
            }
            // check existing busbar/bus
            controlBus(network, vl, bbsOrBusId);
        }
    }

    public void controlVoltageLevelCreation(VoltageLevelCreationInfos voltageLevelCreationInfos, Network network) {
        if (network.getVoltageLevel(voltageLevelCreationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, voltageLevelCreationInfos.getEquipmentId());
        }
        if (voltageLevelCreationInfos.getCouplingDevices().stream()
                .anyMatch(cd -> cd.getBusbarSectionId1().equals(cd.getBusbarSectionId2()))) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR,
                    "Coupling between same bus bar section is not allowed");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && voltageLevelCreationInfos.getIpMin() < 0) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMin must be positive");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMax()) && voltageLevelCreationInfos.getIpMax() < 0) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax must be positive");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && Objects.isNull(voltageLevelCreationInfos.getIpMax())) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required");
        }
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && Objects.nonNull(voltageLevelCreationInfos.getIpMax())
            && voltageLevelCreationInfos.getIpMin() > voltageLevelCreationInfos.getIpMax()) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMin cannot be greater than IpMax");
        }
    }

    private boolean checkBbs(Network network, String busbarSectionId1, String busbarSectionId2, ReportNode subReportNode) {
        Identifiable<?> busOrBbs1 = network.getIdentifiable(busbarSectionId1);
        Identifiable<?> busOrBbs2 = network.getIdentifiable(busbarSectionId2);
        if (busOrBbs1 == null) {
            subReportNode.newReportNode()
                    .withMessageTemplate("notFoundBurOrBusbarSection", "Bus or busbar section ID ${busbarSectionId} not found. Coupler was not created.")
                    .withUntypedValue(BUS_BAR_SECTION_ID, busbarSectionId1)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
            return false;
        }
        if (busOrBbs2 == null) {
            subReportNode.newReportNode()
                    .withMessageTemplate("notFoundBurOrBusbarSection", "Bus or busbar section ID ${busbarSectionId} not found. Coupler was not created.")
                    .withUntypedValue(BUS_BAR_SECTION_ID, busbarSectionId2)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .add();
            return false;
        }
        return true;
    }

    void createVoltageLevel(VoltageLevelCreationInfos voltageLevelCreationInfos,
                                   ReportNode subReportNode, Network network) {
        String substationId = voltageLevelCreationInfos.getSubstationId();
        Substation substation = network.getSubstation(substationId);
        if (substation == null) {
            throw new NetworkModificationException(SUBSTATION_NOT_FOUND, substationId);
        }

        VoltageLevel voltageLevel = substation.newVoltageLevel()
            .setId(voltageLevelCreationInfos.getEquipmentId())
            .setName(voltageLevelCreationInfos.getEquipmentName())
            .setTopologyKind(TopologyKind.NODE_BREAKER)
            .setNominalV(voltageLevelCreationInfos.getNominalV())
            .add();

        if (voltageLevelCreationInfos.getLowVoltageLimit() != null) {
            voltageLevel.setLowVoltageLimit(voltageLevelCreationInfos.getLowVoltageLimit());
        }
        if (voltageLevelCreationInfos.getHighVoltageLimit() != null) {
            voltageLevel.setHighVoltageLimit(voltageLevelCreationInfos.getHighVoltageLimit());
        }

        if (voltageLevelCreationInfos.getIpMax() != null && voltageLevelCreationInfos.getIpMin() != null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMin(voltageLevelCreationInfos.getIpMin())
                    .withIpMax(voltageLevelCreationInfos.getIpMax())
                    .add();
        } else if (voltageLevelCreationInfos.getIpMax() != null && voltageLevelCreationInfos.getIpMin() == null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMax(voltageLevelCreationInfos.getIpMax())
                    .add();
        } else if (voltageLevelCreationInfos.getIpMax() == null && voltageLevelCreationInfos.getIpMin() != null) {
            voltageLevel.newExtension(IdentifiableShortCircuitAdder.class)
                    .withIpMin(voltageLevelCreationInfos.getIpMin())
                    .add();
        }

        CreateVoltageLevelTopologyBuilder voltageLevelTopologyBuilder = new CreateVoltageLevelTopologyBuilder();
        voltageLevelTopologyBuilder.withVoltageLevelId(voltageLevelCreationInfos.getEquipmentId())
                .withAlignedBusesOrBusbarCount(voltageLevelCreationInfos.getBusbarCount())
                .withSectionCount(voltageLevelCreationInfos.getSectionCount())
                .withSwitchKinds(voltageLevelCreationInfos.getSwitchKinds())
                .build().apply(network);

        voltageLevelCreationInfos.getCouplingDevices().forEach(couplingDevice -> {
            if (!checkBbs(network, couplingDevice.getBusbarSectionId1(), couplingDevice.getBusbarSectionId2(), subReportNode)) {
                return;
            }
            CreateCouplingDeviceBuilder couplingDeviceBuilder = new CreateCouplingDeviceBuilder();
            couplingDeviceBuilder.withBusOrBusbarSectionId1(couplingDevice.getBusbarSectionId1())
                .withBusOrBusbarSectionId2(couplingDevice.getBusbarSectionId2())
                .withSwitchPrefixId(voltageLevelCreationInfos.getEquipmentId() + "_COUPL")
                    .build().apply(network, subReportNode);
        });

        subReportNode.newReportNode()
                .withMessageTemplate("voltageLevelCreated", "New voltage level with id=${id} created")
                .withUntypedValue("id", voltageLevelCreationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    public LineAdder createLineAdder(Network network, VoltageLevel voltageLevel1, VoltageLevel voltageLevel2, LineCreationInfos lineCreationInfos, boolean withSwitch1, boolean withSwitch2) {

        // common settings
        LineAdder lineAdder = network.newLine()
                .setId(lineCreationInfos.getEquipmentId())
                .setName(lineCreationInfos.getEquipmentName())
                .setVoltageLevel1(lineCreationInfos.getVoltageLevelId1())
                .setVoltageLevel2(lineCreationInfos.getVoltageLevelId2())
                .setR(lineCreationInfos.getR())
                .setX(lineCreationInfos.getX())
                .setG1(lineCreationInfos.getG1() != null ? lineCreationInfos.getG1() : 0.0)
                .setB1(lineCreationInfos.getB1() != null ? lineCreationInfos.getB1() : 0.0)
                .setG2(lineCreationInfos.getG2() != null ? lineCreationInfos.getG2() : 0.0)
                .setB2(lineCreationInfos.getB2() != null ? lineCreationInfos.getB2() : 0.0);

        // lineAdder completion by topology
        setBranchAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, TwoSides.ONE, withSwitch1);
        setBranchAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, TwoSides.TWO, withSwitch2);

        return lineAdder;
    }

    void setBranchAdderNodeOrBus(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, BranchCreationInfos branchCreationInfos,
                                 TwoSides side, boolean withSwitch) {
        String busOrBusbarSectionId = (side == TwoSides.ONE) ? branchCreationInfos.getBusOrBusbarSectionId1() : branchCreationInfos.getBusOrBusbarSectionId2();
        if (voltageLevel.getTopologyKind() == TopologyKind.BUS_BREAKER) {
            setBranchAdderBusBreaker(branchAdder, voltageLevel, side, busOrBusbarSectionId);
        } else {
            if (withSwitch) { // NODE_BREAKER
                setBranchAdderNodeBreaker(branchAdder, voltageLevel, branchCreationInfos, side, busOrBusbarSectionId);
            }
        }
    }

    private void setBranchAdderBusBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, TwoSides side, String busId) {
        Bus bus = getBusBreakerBus(voltageLevel, busId);

        // complete the lineAdder
        if (side == TwoSides.ONE) {
            branchAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
        } else {
            branchAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
        }
    }

    private void setBranchAdderNodeBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel,
                                           BranchCreationInfos branchCreationInfos, TwoSides side,
                                           String currentBusBarSectionId) {
        // create cell switches
        String sideSuffix = side != null ? "_" + side.name() : "";
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel,
            currentBusBarSectionId,
            branchCreationInfos.getEquipmentId(),
            branchCreationInfos.getEquipmentName(),
            sideSuffix);

        // complete the lineAdder
        if (side == TwoSides.ONE) {
            branchAdder.setNode1(nodeNum);
        } else {
            branchAdder.setNode2(nodeNum);
        }
    }

    public static void createReport(ReportNode reportNode, String reporterKey, String defaultMessage, Map<String, Object> values, TypedValue errorSeverity) {
        ReportNodeAdder adder = reportNode.newReportNode()
                .withMessageTemplate(reporterKey, defaultMessage)
                .withSeverity(errorSeverity);

        for (Map.Entry<String, Object> valueEntry : values.entrySet()) {
            adder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        adder.add();
    }

    public static <T> Predicate<T> distinctByKey(
            Function<? super T, ?> keyExtractor) {

        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    public <T> ReportNode applyElementaryModificationsAndReturnReport(Consumer<T> setter, Supplier<T> getter,
                                                                  AttributeModification<T> modification, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            return buildModificationReport(oldValue, newValue, fieldName);
        }
        return null;
    }

    public <T> ReportNode applyElementaryModificationsAndReturnReport(Consumer<T> setter, Supplier<T> getter,
                                                                                AttributeModification<T> modification, String fieldName, int indentationLevel) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            return buildModificationReportWithIndentation(oldValue, newValue, fieldName, indentationLevel);
        }
        return null;
    }

    public ReportNode createEnabledDisabledReport(String key, boolean enabled) {
        return ReportNode.newRootReportNode()
                .withMessageTemplate(key, "    ${status}")
                .withUntypedValue("status", enabled ? "Enabled" : "Disabled")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public ReportNode reportModifications(ReportNode subReportNode, List<ReportNode> reports, String subReportNodeKey,
                                        String subReportNodeDefaultMessage, Map<String, Object> values) {
        List<ReportNode> validReports = reports.stream().filter(Objects::nonNull).toList();
        ReportNode modificationSubReportNode = null;
        if (!validReports.isEmpty() && subReportNode != null) {
            modificationSubReportNode = subReportNode.newReportNode().withMessageTemplate(subReportNodeKey, subReportNodeDefaultMessage).add();
            ReportNodeAdder adder = modificationSubReportNode.newReportNode().withMessageTemplate(subReportNodeKey, subReportNodeDefaultMessage).withSeverity(TypedValue.INFO_SEVERITY);
            for (Map.Entry<String, Object> valueEntry : values.entrySet()) {
                adder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
            }
            adder.add();
            for (ReportNode report : validReports) {
                ReportNodeAdder reportNodeAdder = modificationSubReportNode.newReportNode().withMessageTemplate(report.getMessageKey(), report.getMessageTemplate()).withSeverity(TypedValue.INFO_SEVERITY);
                for (Map.Entry<String, TypedValue> valueEntry : report.getValues().entrySet()) {
                    reportNodeAdder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
                }
                TypedValue severity = report.getValue(ReportConstants.SEVERITY_KEY).orElse(null);
                if (severity != null) {
                    reportNodeAdder.withSeverity(severity);
                }
                reportNodeAdder.add();
            }
        }
        return modificationSubReportNode;
    }

    public <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
            AttributeModification<T> modification,
            ReportNode subReportNode, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            insertReportNode(subReportNode, buildModificationReport(oldValue, newValue, fieldName));
        }
    }

    public <T> ReportNode applyAndBuildModificationReport(Consumer<T> setter, Supplier<T> getter, AttributeModification<T> modification, String fieldName) {
        T oldValue = getter.get();
        T newValue = modification.applyModification(oldValue);
        setter.accept(newValue);
        return buildModificationReport(oldValue, newValue, fieldName, 1, TypedValue.INFO_SEVERITY);
    }

    public <T> ReportNode buildModificationReport(T oldValue, T newValue, String fieldName) {
        return buildModificationReport(oldValue, newValue, fieldName, 1, TypedValue.INFO_SEVERITY);
    }

    //TODO rename to buildModificationReport()
    public <T> ReportNode buildModificationReportWithIndentation(T oldValue, T newValue, String fieldName, int indentationLevel) {
        return buildModificationReport(oldValue, newValue, fieldName, indentationLevel, TypedValue.INFO_SEVERITY);
    }

    static <T> ReportNode buildModificationReport(T oldValue, T newValue, String fieldName, int indentationLevel, TypedValue severity) {
        final String oldValueString = (oldValue == null || oldValue instanceof Double oldDouble && Double.isNaN(oldDouble))
                ? NO_VALUE : oldValue.toString();
        final String newValueString = (newValue == null || newValue instanceof Double newDouble && Double.isNaN(newDouble))
                ? NO_VALUE : newValue.toString();
        final String indentation = "\t".repeat(indentationLevel);
        return ReportNode.newRootReportNode()
                .withMessageTemplate("modification-indent" + indentationLevel, indentation + "${fieldName} : ${oldValue} → ${newValue}")
                .withUntypedValue("fieldName", fieldName)
                .withUntypedValue("oldValue", oldValueString)
                .withUntypedValue("newValue", newValueString)
                .withSeverity(severity)
                .build();
    }

    public Terminal getTerminalFromIdentifiable(Network network, String equipmentId, String type, String voltageLevelId) {
        if (network != null && equipmentId != null && type != null && voltageLevelId != null) {
            Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, IdentifiableType.valueOf(type), equipmentId);

            if (identifiable == null) {
                throw new NetworkModificationException(EQUIPMENT_NOT_FOUND, "Equipment with id=" + equipmentId + " not found with type " + type);
            }

            if (identifiable instanceof Injection<?>) {
                return ((Injection<?>) identifiable).getTerminal();
            } else if (identifiable instanceof Branch<?>) {
                return ((Branch<?>) identifiable).getTerminal(voltageLevelId);
            }
        }

        return null;
    }

    public List<Terminal> getTerminalsFromIdentifiable(Identifiable<?> identifiable) {
        if (identifiable instanceof Branch<?> branch) {
            return Stream.of(
                    branch.getTerminal1(),
                    branch.getTerminal2()
            ).toList();
        } else if (identifiable instanceof ThreeWindingsTransformer w3t) {
            return Stream.of(
                    w3t.getLeg1().getTerminal(),
                    w3t.getLeg2().getTerminal(),
                    w3t.getLeg3().getTerminal()
            ).toList();
        } else if (identifiable instanceof HvdcLine hvdcLine) {
            return Stream.of(
                    hvdcLine.getConverterStation1().getTerminal(),
                    hvdcLine.getConverterStation2().getTerminal()
            ).toList();
        }
        throw NetworkModificationException.createEquipmentTypeNotSupported(identifiable.getClass().getSimpleName());
    }

    public static boolean isInjectionConnected(Injection<?> injection) {
        return injection != null && injection.getTerminal().isConnected();
    }

    public void disconnectCreatedInjection(InjectionCreationInfos modificationInfos, Injection<?> injection, ReportNode subReportNode) {
        // A newly created injection is connected by default, unless we choose not to do
        if (!modificationInfos.isTerminalConnected()) {
            injection.getTerminal().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate("equipmentDisconnected", "Equipment with id=${id} disconnected")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    public ReportNode modifyInjectionConnectivityAttributes(ConnectablePosition<?> connectablePosition,
                                                      ConnectablePositionAdder<?> connectablePositionAdder,
                                                      Injection<?> injection,
                                                      InjectionModificationInfos modificationInfos,
                                                      ReportNode connectivityReports) {
        List<ReportNode> reports = new ArrayList<>();
        if (modificationInfos.getVoltageLevelId() == null || modificationInfos.getBusOrBusbarSectionId() == null) {
            return ReportNode.newRootReportNode()
                    .withMessageTemplate("VoltageLevelOrBusbarSectionNotFound",
                            "Voltage level id or Bus bar section id of equipment id=${id} not found")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build();
        }
        if (connectablePosition != null) {
            modifyExistingConnectivityPosition(connectablePosition, modificationInfos, reports);
        } else {
            createNewConnectivityPosition(connectablePositionAdder, modificationInfos, reports);
        }
        modifyInjectionConnection(modificationInfos, injection, reports);
        return reportModifications(connectivityReports, reports, "ConnectivityModified", CONNECTIVITY, Map.of());
    }

    public ReportNode modifyBranchConnectivityAttributes(ConnectablePosition<?> connectablePosition,
                                                         ConnectablePositionAdder<?> connectablePositionAdder,
                                                         Branch<?> branch,
                                                         BranchModificationInfos modificationInfos,
                                                         ReportNode connectivityReports) {
        List<ReportNode> reports = new ArrayList<>();
        if (modificationInfos.getVoltageLevelId1() == null || modificationInfos.getBusOrBusbarSectionId1() == null) {
            return ReportNode.newRootReportNode()
                    .withMessageTemplate("VoltageLevelOrBusbarSectionNotFound",
                            "Voltage level id or Bus bar section id of equipment id=${id} not found")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build();
        }
        if (connectablePosition != null) {
            modifyExistingBranchConnectivityPosition(connectablePosition, modificationInfos, reports);
        } else {
            createNewBranchConnectivityPosition(connectablePositionAdder, modificationInfos, reports);
        }
        modifyBranchConnection1(modificationInfos, branch, reports);
        modifyBranchConnection2(modificationInfos, branch, reports);
        return reportModifications(connectivityReports, reports, "ConnectivityModified", CONNECTIVITY, Map.of());
    }

    private void modifyExistingBranchConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                                    BranchModificationInfos modificationInfos,
                                                    List<ReportNode> reports) {
        ConnectablePosition.Feeder feeder1 = connectablePosition.getFeeder1();
        ConnectablePosition.Feeder feeder2 = connectablePosition.getFeeder2();
        ReportNode connectionName1Report = applyElementaryModificationsAndReturnReport(
                feeder1::setName,
                feeder1.getName()::get,
                modificationInfos.getConnectionName1(),
                CONNECTION_NAME_FIELD_NAME_1);
        if (connectionName1Report != null) {
            reports.add(connectionName1Report);
        }
        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(
                feeder1::setDirection,
                feeder1::getDirection,
                modificationInfos.getConnectionDirection1(),
                CONNECTION_DIRECTION_FIELD_NAME_1);
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }
        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(
                feeder1::setOrder,
                feeder1.getOrder()::get,
                modificationInfos.getConnectionPosition1(),
                CONNECTION_POSITION_FIELD_NAME_1);
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }
        ReportNode connectionName2Report = applyElementaryModificationsAndReturnReport(
                feeder2::setName,
                feeder2.getName()::get,
                modificationInfos.getConnectionName2(),
                CONNECTION_NAME_FIELD_NAME_2);
        if (connectionName2Report != null) {
            reports.add(connectionName2Report);
        }
        ReportNode connectionDirection2Report = applyElementaryModificationsAndReturnReport(
                feeder2::setDirection,
                feeder2::getDirection,
                modificationInfos.getConnectionDirection2(),
                CONNECTION_DIRECTION_FIELD_NAME_2);
        if (connectionDirection2Report != null) {
            reports.add(connectionDirection2Report);
        }
        ReportNode connectionPosition2Report = applyElementaryModificationsAndReturnReport(
                feeder2::setOrder,
                feeder2.getOrder()::get,
                modificationInfos.getConnectionPosition2(),
                CONNECTION_POSITION_FIELD_NAME_2);
        if (connectionPosition2Report != null) {
            reports.add(connectionPosition2Report);
        }
    }

    private void modifyExistingConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                                  InjectionModificationInfos modificationInfos,
                                                  List<ReportNode> reports) {
        ConnectablePosition.Feeder feeder1 = connectablePosition.getFeeder1();
        ConnectablePosition.Feeder feeder2 = connectablePosition.getFeeder2();
        ReportNode connectionName1Report = applyElementaryModificationsAndReturnReport(
                feeder1::setName,
                feeder1.getName()::get,
                modificationInfos.getConnectionName(),
                CONNECTION_NAME_FIELD_NAME);
        if (connectionName1Report != null) {
            reports.add(connectionName1Report);
        }
        ReportNode connectionDirection1Report = applyElementaryModificationsAndReturnReport(
                feeder1::setDirection,
                feeder1::getDirection,
                modificationInfos.getConnectionDirection(),
                CONNECTION_DIRECTION_FIELD_NAME);
        if (connectionDirection1Report != null) {
            reports.add(connectionDirection1Report);
        }
        ReportNode connectionPosition1Report = applyElementaryModificationsAndReturnReport(
                feeder1::setOrder,
                feeder1.getOrder()::get,
                modificationInfos.getConnectionPosition(),
                CONNECTION_POSITION_FIELD_NAME);
        if (connectionPosition1Report != null) {
            reports.add(connectionPosition1Report);
        }
        ReportNode connectionName2Report = applyElementaryModificationsAndReturnReport(
                feeder2::setName,
                feeder2.getName()::get,
                modificationInfos.getConnectionName(),
                CONNECTION_NAME_FIELD_NAME_1);
        if (connectionName2Report != null) {
            reports.add(connectionName2Report);
        }
        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(
                feeder1::setDirection,
                feeder1::getDirection,
                modificationInfos.getConnectionDirection(),
                CONNECTION_DIRECTION_FIELD_NAME);
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }
        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(
                feeder1::setOrder,
                feeder1.getOrder()::get,
                modificationInfos.getConnectionPosition(),
                CONNECTION_POSITION_FIELD_NAME);
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }
    }

    private void createNewConnectivityPosition(ConnectablePositionAdder<?> adder,
                                             InjectionModificationInfos modificationInfos,
                                             List<ReportNode> reports) {
        ConnectablePositionAdder.FeederAdder<?> feeder = adder.newFeeder();
        ReportNode connectionNameReport = applyElementaryModificationsAndReturnReport(
                feeder::withName,
                () -> null,
                modificationInfos.getConnectionName(),
                CONNECTION_NAME_FIELD_NAME);
        if (connectionNameReport != null) {
            reports.add(connectionNameReport);
        }

        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(
                feeder::withDirection,
                () -> null,
                modificationInfos.getConnectionDirection(),
                CONNECTION_DIRECTION_FIELD_NAME);
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }

        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(
                feeder::withOrder,
                () -> null,
                modificationInfos.getConnectionPosition(),
                CONNECTION_POSITION_FIELD_NAME);
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }

        // Finalize by adding the feeder
        adder.add();
    }

    private void createNewBranchConnectivityPosition(ConnectablePositionAdder<?> adder,
                                                     BranchModificationInfos modificationInfos,
                                               List<ReportNode> reports) {
        ConnectablePositionAdder.FeederAdder<?> feeder1 = adder.newFeeder1();
        ConnectablePositionAdder.FeederAdder<?> feeder2 = adder.newFeeder2();
        ReportNode connectionName1Report = applyElementaryModificationsAndReturnReport(
                feeder1::withName,
                () -> null,
                modificationInfos.getConnectionName1(),
                CONNECTION_NAME_FIELD_NAME_1);
        if (connectionName1Report != null) {
            reports.add(connectionName1Report);
        }

        ReportNode connectionDirection1Report = applyElementaryModificationsAndReturnReport(
                feeder1::withDirection,
                () -> null,
                modificationInfos.getConnectionDirection1(),
                CONNECTION_DIRECTION_FIELD_NAME_1);
        if (connectionDirection1Report != null) {
            reports.add(connectionDirection1Report);
        }

        ReportNode connectionPosition2Report = applyElementaryModificationsAndReturnReport(
                feeder1::withOrder,
                () -> null,
                modificationInfos.getConnectionPosition1(),
                CONNECTION_POSITION_FIELD_NAME_1);
        if (connectionPosition2Report != null) {
            reports.add(connectionPosition2Report);
        }

        ReportNode connectionName2Report = applyElementaryModificationsAndReturnReport(
                feeder1::withName,
                () -> null,
                modificationInfos.getConnectionName2(),
                CONNECTION_NAME_FIELD_NAME_2);
        if (connectionName2Report != null) {
            reports.add(connectionName2Report);
        }

        ReportNode connectionDirection2Report = applyElementaryModificationsAndReturnReport(
                feeder1::withDirection,
                () -> null,
                modificationInfos.getConnectionDirection2(),
                CONNECTION_DIRECTION_FIELD_NAME_2);
        if (connectionDirection2Report != null) {
            reports.add(connectionDirection2Report);
        }

        ReportNode connectionPosition3Report = applyElementaryModificationsAndReturnReport(
                feeder1::withOrder,
                () -> null,
                modificationInfos.getConnectionPosition2(),
                CONNECTION_POSITION_FIELD_NAME_2);
        if (connectionPosition3Report != null) {
            reports.add(connectionPosition3Report);
        }

        // Finalize by adding the feeder
        adder.add();
    }

    public void modifyBranchConnection1(BranchModificationInfos modificationInfos, Branch<?> branch, List<ReportNode> subReportNode) {
        if (modificationInfos.getTerminal1Connected() != null && branch != null) {
            if (branch.getTerminal1().isConnected() && Boolean.FALSE.equals(modificationInfos.getTerminal1Connected().getValue())) {
                branch.getTerminal1().disconnect();
                if (branch.getTerminal1().isConnected()) {
                    throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                            String.format("Could not disconnect equipment '%s'", branch.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } else if (!branch.getTerminal1().isConnected() && Boolean.TRUE.equals(modificationInfos.getTerminal1Connected().getValue())) {
                branch.getTerminal1().connect();
                if (!branch.getTerminal1().isConnected()) {
                    throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                            String.format("Could not connect equipment '%s'", branch.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentConnected", "    Equipment with id=${id} connected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
    }

    public void modifyBranchConnection2(BranchModificationInfos modificationInfos, Branch<?> branch, List<ReportNode> subReportNode) {
        if (modificationInfos.getTerminal2Connected() != null && branch != null) {
            if (branch.getTerminal2().isConnected() && Boolean.FALSE.equals(modificationInfos.getTerminal2Connected().getValue())) {
                branch.getTerminal2().disconnect();
                if (branch.getTerminal2().isConnected()) {
                    throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                            String.format("Could not disconnect equipment '%s'", branch.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } else if (!branch.getTerminal2().isConnected() && Boolean.TRUE.equals(modificationInfos.getTerminal2Connected().getValue())) {
                branch.getTerminal2().connect();
                if (!branch.getTerminal2().isConnected()) {
                    throw new NetworkModificationException(BRANCH_MODIFICATION_ERROR,
                            String.format("Could not connect equipment '%s'", branch.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentConnected", "    Equipment with id=${id} connected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
    }

    public void modifyInjectionConnection(InjectionModificationInfos modificationInfos, Injection<?> injection, List<ReportNode> subReportNode) {
        if (modificationInfos.getTerminalConnected() != null && injection != null) {
            if (isInjectionConnected(injection) && Boolean.FALSE.equals(modificationInfos.getTerminalConnected().getValue())) {
                injection.getTerminal().disconnect();
                if (isInjectionConnected(injection)) {
                    throw new NetworkModificationException(INJECTION_MODIFICATION_ERROR,
                        String.format("Could not disconnect equipment '%s'", injection.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            } else if (!isInjectionConnected(injection) && Boolean.TRUE.equals(modificationInfos.getTerminalConnected().getValue())) {
                injection.getTerminal().connect();
                if (!isInjectionConnected(injection)) {
                    throw new NetworkModificationException(INJECTION_MODIFICATION_ERROR,
                        String.format("Could not connect equipment '%s'", injection.getId()));
                }
                subReportNode.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentConnected", "    Equipment with id=${id} connected")
                        .withUntypedValue("id", modificationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
        }
    }

    public void disconnectBranch(BranchCreationInfos modificationInfos, Branch<?> branch, ReportNode subReportNode) {
        // A newly created branch is connected by default on both sides, unless we choose not to do
        if (!modificationInfos.isConnected1()) {
            branch.getTerminal1().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate("terminal1Disconnected", "Equipment with id=${id} disconnected on side 1")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (!modificationInfos.isConnected2()) {
            branch.getTerminal2().disconnect();
            subReportNode.newReportNode()
                    .withMessageTemplate("terminal2Disconnected", "Equipment with id=${id} disconnected on side 2")
                    .withUntypedValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
    }

    public Identifiable<?> getEquipmentByIdentifiableType(Network network, IdentifiableType type, String equipmentId) {
        if (type == null || equipmentId == null) {
            return null;
        }

        switch (type) {
            case HVDC_LINE:
                return network.getHvdcLine(equipmentId);
            case LINE:
                return network.getLine(equipmentId);
            case TWO_WINDINGS_TRANSFORMER:
                return network.getTwoWindingsTransformer(equipmentId);
            case THREE_WINDINGS_TRANSFORMER:
                return network.getThreeWindingsTransformer(equipmentId);
            case GENERATOR:
                return network.getGenerator(equipmentId);
            case LOAD:
                return network.getLoad(equipmentId);
            case BATTERY:
                return network.getBattery(equipmentId);
            case SHUNT_COMPENSATOR:
                return network.getShuntCompensator(equipmentId);
            case STATIC_VAR_COMPENSATOR:
                return network.getStaticVarCompensator(equipmentId);
            case DANGLING_LINE:
                return network.getDanglingLine(equipmentId);
            case HVDC_CONVERTER_STATION:
                return network.getHvdcConverterStation(equipmentId);
            case SUBSTATION:
                return network.getSubstation(equipmentId);
            case VOLTAGE_LEVEL:
                return network.getVoltageLevel(equipmentId);
            case BUSBAR_SECTION:
                return network.getBusbarSection(equipmentId);
            default:
                return null;
        }
    }

    public void setCurrentLimits(CurrentLimitsInfos currentLimitsInfos, CurrentLimitsAdder limitsAdder) {
        if (currentLimitsInfos != null) {
            boolean hasPermanent = currentLimitsInfos.getPermanentLimit() != null;
            boolean hasTemporary = currentLimitsInfos.getTemporaryLimits() != null && !currentLimitsInfos.getTemporaryLimits().isEmpty();
            if (hasPermanent) {
                limitsAdder.setPermanentLimit(currentLimitsInfos.getPermanentLimit());
            }
            if (hasTemporary) {
                for (CurrentTemporaryLimitCreationInfos limit : currentLimitsInfos.getTemporaryLimits()) {
                    limitsAdder
                            .beginTemporaryLimit()
                            .setName(limit.getName())
                            .setValue(limit.getValue() == null ? Double.MAX_VALUE : limit.getValue())
                            .setAcceptableDuration(limit.getAcceptableDuration() == null ? Integer.MAX_VALUE : limit.getAcceptableDuration())
                            .endTemporaryLimit();
                }
            }
            if (hasPermanent || hasTemporary) {
                limitsAdder.add();
            }
        }
    }

    public <T> ReportNode buildCreationReport(T value, String fieldName) {
        String newValueString = value == null ? NO_VALUE : value.toString();
        return ReportNode.newRootReportNode()
                .withMessageTemplate("Creation" + fieldName, "    ${fieldName} : ${value}")
                .withUntypedValue("fieldName", fieldName)
                .withUntypedValue("value", newValueString)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public <T> void reportElementaryCreation(ReportNode subReportNode, T value, String fieldName) {
        insertReportNode(subReportNode, buildCreationReport(value, fieldName));
    }

    public String formatRegulationModeReport(PhaseTapChanger.RegulationMode regulationMode) {
        switch (regulationMode) {
            case FIXED_TAP:
                return "    Fixed tap";
            case CURRENT_LIMITER :
                return "    Current limiter";
            case ACTIVE_POWER_CONTROL :
                return "    Active power control";
            default :
                return "";

        }
    }

    public void modifyReactiveCapabilityCurvePoints(Collection<ReactiveCapabilityCurve.Point> points,
                                                    List<ReactiveCapabilityCurveModificationInfos> modificationPoints,
                                                    ReactiveCapabilityCurveAdder adder,
                                                    ReportNode subReportNode, ReportNode subReportNodeLimits) {
        List<ReportNode> reports = new ArrayList<>();
        List<ReactiveCapabilityCurve.Point> equipementIdPoints = new ArrayList<>(points);
        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurve.Point oldPoint = i < equipementIdPoints.size() - 1 ? equipementIdPoints.get(i) : null;
                    ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (modificationPoints.size() - 1)) {
                        fieldSuffix = "max";
                        if (!CollectionUtils.isEmpty(equipementIdPoints)) {
                            oldPoint = equipementIdPoints.get(equipementIdPoints.size() - 1);
                        }
                    } else {
                        fieldSuffix = Integer.toString(i);
                    }
                    createReactiveCapabilityCurvePoint(adder, newPoint, oldPoint, reports, fieldSuffix);
                });
        adder.add();
        ReportNode subReportNodeReactiveLimits = null;
        ReportNode subReporterLimits2 = subReportNodeLimits;
        if (subReportNodeLimits == null && !reports.isEmpty()) {
            subReporterLimits2 = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            subReporterLimits2.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            subReporterLimits2.newReportNode()
                    .withMessageTemplate(LIMITS, LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (subReporterLimits2 != null && !reports.isEmpty()) {
            subReportNodeReactiveLimits = subReporterLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
            subReportNodeReactiveLimits.newReportNode()
                    .withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        reportModifications(subReportNodeReactiveLimits, reports, "curveReactiveLimitsModified", "By diagram", Map.of());
    }

    public void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                    ReactiveCapabilityCurveModificationInfos newPoint,
                                                    ReactiveCapabilityCurve.Point oldPoint,
                                                    List<ReportNode> reports,
                                                    String fieldSuffix) {
        Double oldMaxQ = Double.NaN;
        Double oldMinQ = Double.NaN;
        Double oldP = Double.NaN;
        if (oldPoint != null) {
            oldMaxQ = oldPoint.getMaxQ();
            oldMinQ = oldPoint.getMinQ();
            oldP = oldPoint.getP();
        }
        var maxQ = newPoint.getMaxQ() != null ? newPoint.getMaxQ() : oldMaxQ;
        var minQ = newPoint.getMinQ() != null ? newPoint.getMinQ() : oldMinQ;
        var p = newPoint.getP() != null ? newPoint.getP() : oldP;

        adder.beginPoint()
                .setMaxQ(maxQ)
                .setMinQ(minQ)
                .setP(p)
                .endPoint();
        addToReports(reports, p, oldP, "P" + fieldSuffix);
        addToReports(reports, minQ, oldMinQ, "QminP" + fieldSuffix);
        addToReports(reports, maxQ, oldMaxQ, "QmaxP" + fieldSuffix);
    }

    public void addToReports(List<ReportNode> reports, Double newValue, Double oldValue, String fieldName) {
        if (newValue != null) {
            reports.add(buildModificationReport(oldValue, newValue, fieldName));
        }
    }

    public void modifyMinMaxReactiveLimits(AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower, ReactiveLimitsHolder reactiveLimitsHolder,
                                           ReportNode subReportNode, ReportNode subReportNodeLimits) {
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = reactiveLimitsHolder.getReactiveLimits();
        MinMaxReactiveLimitsAdder newMinMaxReactiveLimitsAdder = reactiveLimitsHolder.newMinMaxReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = reactiveLimitsHolder.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }
        modifyMinMaxReactiveLimits(minMaxReactiveLimits,
                newMinMaxReactiveLimitsAdder, subReportNode, subReportNodeLimits,
                minimumReactivePower,
                maximumReactivePower);
    }

    public void modifyMinMaxReactiveLimits(MinMaxReactiveLimits minMaxReactiveLimits, MinMaxReactiveLimitsAdder newMinMaxReactiveLimits,
                                           ReportNode subReportNode, ReportNode subReportNodeLimits, AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower) {
        List<ReportNode> reports = new ArrayList<>();

        if (minimumReactivePower != null
                && maximumReactivePower != null) {
            newMinMaxReactiveLimits.setMinQ(minimumReactivePower.getValue())
                    .setMaxQ(maximumReactivePower.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    minimumReactivePower.getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    maximumReactivePower.getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minimumReactivePower != null) {
            newMinMaxReactiveLimits.setMinQ(minimumReactivePower.getValue())
                    .setMaxQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    minimumReactivePower.getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
        } else if (maximumReactivePower != null) {
            newMinMaxReactiveLimits
                    .setMinQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : -Double.MAX_VALUE)
                    .setMaxQ(maximumReactivePower.getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    maximumReactivePower.getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minMaxReactiveLimits == null) {
            newMinMaxReactiveLimits.setMinQ(-Double.MAX_VALUE)
                    .setMaxQ(Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    -Double.MAX_VALUE,
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(Double.NaN,
                    Double.MAX_VALUE,
                    MAX_REACTIVE_POWER_FIELDNAME));
        }
        ReportNode subReportNodeReactiveLimits = null;
        ReportNode subReportNodeLimits2 = subReportNodeLimits;
        if (subReportNodeLimits == null && !reports.isEmpty()) {
            subReportNodeLimits2 = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            subReportNodeLimits2.newReportNode()
                    .withMessageTemplate(LIMITS, LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        if (subReportNodeLimits2 != null && !reports.isEmpty()) {
            subReportNodeReactiveLimits = subReportNodeLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
            subReportNodeReactiveLimits.newReportNode()
                    .withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        reportModifications(subReportNodeReactiveLimits, reports, "minMaxReactiveLimitsModified", "By range", Map.of());
    }

    private void modifyExistingActivePowerControl(ActivePowerControl<?> activePowerControl,
                                                  AttributeModification<Boolean> participateInfo,
                                                  AttributeModification<Float> droopInfo,
                                                  List<ReportNode> reports) {
        double oldDroop = activePowerControl.getDroop();
        boolean oldParticipate = activePowerControl.isParticipate();

        Optional.ofNullable(participateInfo).ifPresent(info -> {
            activePowerControl.setParticipate(info.getValue());
            reports.add(buildModificationReport(oldParticipate, info.getValue(), "Participate"));
        });

        Optional.ofNullable(droopInfo).ifPresent(info -> {
            activePowerControl.setDroop(info.getValue());
            reports.add(buildModificationReport(oldDroop, info.getValue(), "Droop"));
        });
    }

    private void createNewActivePowerControl(ActivePowerControlAdder<?> adder,
                                             AttributeModification<Boolean> participateInfo,
                                             AttributeModification<Float> droopInfo,
                                             List<ReportNode> reports) {
        boolean participate = participateInfo != null ? participateInfo.getValue() : false;
        adder.withParticipate(participate);
        if (participateInfo != null) {
            reports.add(buildModificationReport(null, participate, "Participate"));
        }
        double droop = droopInfo != null ? droopInfo.getValue() : Double.NaN;
        adder.withDroop(droop);
        if (droopInfo != null) {
            reports.add(buildModificationReport(Double.NaN, droop, "Droop"));
        }
        adder.add();
    }

    public ReportNode modifyActivePowerControlAttributes(ActivePowerControl<?> activePowerControl,
                                                       ActivePowerControlAdder<?> activePowerControlAdder,
                                                       AttributeModification<Boolean> participateInfo,
                                                       AttributeModification<Float> droopInfo,
                                                        ReportNode subReportNode,
                                                         ReportNode subReporterSetpoints) {
        List<ReportNode> reports = new ArrayList<>();
        if (activePowerControl != null) {
            modifyExistingActivePowerControl(activePowerControl, participateInfo, droopInfo, reports);
        } else {
            createNewActivePowerControl(activePowerControlAdder, participateInfo, droopInfo, reports);
        }

        ReportNode subReportNodeSetpoints2 = subReporterSetpoints;
        if (subReporterSetpoints == null && !reports.isEmpty()) {
            subReportNodeSetpoints2 = subReportNode.newReportNode().withMessageTemplate(SETPOINTS, SETPOINTS).add();
            subReportNodeSetpoints2.newReportNode()
                    .withMessageTemplate(SETPOINTS, SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();
        }
        reportModifications(subReportNodeSetpoints2, reports, "activePowerRegulationModified", "Active power regulation", Map.of());
        return subReportNodeSetpoints2;
    }

    public void checkMaxQGreaterThanMinQ(
            List<ReactiveCapabilityCurveModificationInfos> modificationPoints,
            NetworkModificationException.Type exceptionType, String errorMessage
    ) {
        for (var point : modificationPoints) {
            double maxQ = Double.NaN;
            double minQ = Double.NaN;

            if (point.getMaxQ() != null) {
                maxQ = point.getMaxQ();
            } else if (point.getOldMaxQ() != null) {
                maxQ = point.getOldMaxQ();
            }

            if (point.getMinQ() != null) {
                minQ = point.getMinQ();
            } else if (point.getOldMinQ() != null) {
                minQ = point.getOldMinQ();
            }

            if (maxQ < minQ) {
                throw new NetworkModificationException(
                    exceptionType,
                    errorMessage + "maximum reactive power " + maxQ + " is expected to be greater than or equal to minimum reactive power " + minQ
                );
            }
        }
    }

    public void checkMaxReactivePowerGreaterThanMinReactivePower(MinMaxReactiveLimits minMaxReactiveLimits, AttributeModification<Double> minimumReactivePowerInfo, AttributeModification<Double> maximumReactivePowerInfo, NetworkModificationException.Type exceptionType, String errorMessage) {
        Double previousMinimumReactivePower = minMaxReactiveLimits.getMinQ();
        Double previousMaximumReactivePower = minMaxReactiveLimits.getMaxQ();
        Double minReactivePower = minimumReactivePowerInfo != null ? minimumReactivePowerInfo.getValue() : previousMinimumReactivePower;
        Double maxReactivePower = maximumReactivePowerInfo != null ? maximumReactivePowerInfo.getValue() : previousMaximumReactivePower;
        if (minReactivePower > maxReactivePower) {
            throw new NetworkModificationException(exceptionType, errorMessage + "maximum reactive power " + maxReactivePower + " is expected to be greater than or equal to minimum reactive power " + minReactivePower);
        }
    }

    public void checkReactiveLimit(ReactiveLimitsHolder reactiveLimitsHolder, AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower,
                                   List<ReactiveCapabilityCurveModificationInfos> modificationPoints, NetworkModificationException.Type exeptionType, String errorMessage) {
        if (reactiveLimitsHolder.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX
                && (minimumReactivePower != null || maximumReactivePower != null)) {
            MinMaxReactiveLimits minMaxReactiveLimits = reactiveLimitsHolder.getReactiveLimits(MinMaxReactiveLimits.class);
            ModificationUtils.getInstance().checkMaxReactivePowerGreaterThanMinReactivePower(minMaxReactiveLimits, minimumReactivePower, maximumReactivePower, exeptionType, errorMessage);
        }
        if (modificationPoints != null) {
            ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(modificationPoints, exeptionType, errorMessage);
        }
    }

    public void checkActivePowerZeroOrBetweenMinAndMaxActivePower(AttributeModification<Double> activePowerInfos, AttributeModification<Double> minActivePowerInfos, AttributeModification<Double> maxActivePowerInfos, Double previousMinActivePower, Double previousMaxActivePower, Double previousActivePower, NetworkModificationException.Type exceptionType, String errorMessage) {
        Double minActivePower = minActivePowerInfos != null ? minActivePowerInfos.getValue() : previousMinActivePower;
        Double maxActivePower = maxActivePowerInfos != null ? maxActivePowerInfos.getValue() : previousMaxActivePower;
        Double activePower = activePowerInfos != null ? activePowerInfos.getValue() : previousActivePower;

        if (activePower != 0 && (activePower < minActivePower || activePower > maxActivePower)) {
            throw new NetworkModificationException(exceptionType, errorMessage + "Active power " + activePower + " is expected to be equal to 0 or within the range of minimum active power and maximum active power: [" + minActivePower + ", " + maxActivePower + "]");
        }
    }

    private NetworkModificationException makeEquipmentException(NetworkModificationException.Type errorType,
                                                                       String equipmentId,
                                                                       String equipmentName,
                                                                       String msgSuffix) {
        return new NetworkModificationException(errorType,
                equipmentName + " '" + equipmentId + "' : " + msgSuffix);
    }

    public void checkReactiveLimitsCreation(ReactiveLimitsHolderInfos modificationInfos,
                                            NetworkModificationException.Type errorType,
                                            String equipmentId,
                                            String equipmentName) {
        // check min max reactive limits
        if (modificationInfos.getMinQ() != null && modificationInfos.getMaxQ() != null) {
            if (Double.isNaN(modificationInfos.getMinQ())) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "minimum reactive power is not set");
            } else if (Double.isNaN(modificationInfos.getMaxQ())) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "maximum reactive power is not set");
            } else if (modificationInfos.getMaxQ() < modificationInfos.getMinQ()) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "maximum reactive power is expected to be greater than or equal to minimum reactive power");
            }
        }

        // check reactive capability curve limits
        List<ReactiveCapabilityCurveCreationInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        if (!org.apache.commons.collections4.CollectionUtils.isEmpty(points)) {
            if (points.size() < 2) {
                throw makeEquipmentException(errorType, equipmentId, equipmentName, "a reactive capability curve should have at least two points");
            }
            IntStream.range(0, points.size())
                    .forEach(i -> {
                        ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                        if (Double.isNaN(newPoint.getP())) {
                            throw makeEquipmentException(errorType, equipmentId, equipmentName, "P is not set in a reactive capability curve limits point");
                        } else if (Double.isNaN(newPoint.getMinQ())) {
                            throw makeEquipmentException(errorType, equipmentId, equipmentName, "min Q is not set in a reactive capability curve limits point");
                        } else if (Double.isNaN(newPoint.getMaxQ())) {
                            throw makeEquipmentException(errorType, equipmentId, equipmentName, "max Q is not set in a reactive capability curve limits point");
                        }
                    });
        }
    }

    public static void addToReports(List<ReportNode> reports, Double newValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildCreationReport(newValue, fieldName));
        }
    }

    public void createReactiveLimits(ReactiveLimitsHolderInfos creationInfos,
                                            ReactiveLimitsHolder reactiveLimitsHolder,
                                     ReportNode subReporter) {
        if (Boolean.TRUE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createReactiveCapabilityCurve(creationInfos, reactiveLimitsHolder, subReporter);
        } else if (Boolean.FALSE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createMinMaxReactiveLimits(creationInfos, reactiveLimitsHolder, subReporter);
        }
    }

    public void createMinMaxReactiveLimits(ReactiveLimitsHolderInfos batteryCreationInfos,
                                                  ReactiveLimitsHolder reactiveLimitsHolder,
                                           ReportNode subReportNode) {
        List<ReportNode> minMaxReactiveLimitsReports = new ArrayList<>();
        if (batteryCreationInfos.getMinQ() != null && batteryCreationInfos.getMaxQ() != null) {
            reactiveLimitsHolder.newMinMaxReactiveLimits()
                    .setMinQ(batteryCreationInfos.getMinQ())
                    .setMaxQ(batteryCreationInfos.getMaxQ())
                    .add();

            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                    batteryCreationInfos.getMinQ(),
                    MIN_REACTIVE_POWER_FIELDNAME));

            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                    batteryCreationInfos.getMaxQ(),
                    MAX_REACTIVE_POWER_FIELDNAME));

            ReportNode subReporterReactiveLimits = subReportNode.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
            subReporterReactiveLimits.newReportNode()
                    .withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .add();

            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "minMaxReactiveLimitsCreated", "By range", Map.of());
        }
    }

    public void createReactiveCapabilityCurve(ReactiveLimitsHolderInfos creationInfos,
                                                     ReactiveLimitsHolder reactiveLimitsHolder,
                                              ReportNode subReportNode) {
        List<ReportNode> pointsReports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = reactiveLimitsHolder.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveCreationInfos> points = creationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (points.size() - 1)) {
                        fieldSuffix = "max";
                    } else {
                        fieldSuffix = Integer.toString(i - 1);
                    }
                    createReactiveCapabilityCurvePoint(adder, newPoint, pointsReports, fieldSuffix);
                });
        adder.add();
        ReportNode subReporterReactiveLimits = subReportNode.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
        subReporterReactiveLimits.newReportNode()
                .withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, pointsReports, "curveReactiveLimitsCreated", "By diagram", Map.of());
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                           ReactiveCapabilityCurveCreationInfos point,
                                                           List<ReportNode> reports,
                                                           String fieldSuffix) {
        adder.beginPoint()
                .setMaxQ(point.getMaxQ())
                .setMinQ(point.getMinQ())
                .setP(point.getP())
                .endPoint();
        addToReports(reports, point.getP(), "P" + fieldSuffix);
        addToReports(reports, point.getMinQ(), "QminP" + fieldSuffix);
        addToReports(reports, point.getMaxQ(), "QmaxP" + fieldSuffix);
    }

    public boolean isValidFilter(ReportNode subReportNode,
                                 NetworkModificationException.Type errorType,
                                 Map<UUID, FilterEquipments> exportFilters) {
        boolean noValidEquipmentId = exportFilters.values().stream()
                .allMatch(filterEquipments -> CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()));

        if (noValidEquipmentId) {
            String errorMsg = "${errorType}: There is no valid equipment ID among the provided filter(s)";
            createReport(subReportNode, "invalidFilters", errorMsg, Map.of("errorType", errorType), TypedValue.ERROR_SEVERITY);
            return false;
        }

        return true;
    }

    public static List<IdentifiableAttributes> getIdentifiableAttributes(Map<UUID, FilterEquipments> exportFilters, Map<UUID, FilterEquipments> filtersWithWrongEquipmentIds, List<FilterInfos> filterInfos, ReportNode subReportNode) {
        filterInfos.stream()
                .filter(f -> !exportFilters.containsKey(f.getId()))
                .forEach(f -> createReport(subReportNode,
                        "filterNotFound",
                        "Cannot find the following filter: ${name}",
                        Map.of("name", f.getName()), TypedValue.WARN_SEVERITY));

        return filterInfos
                .stream()
                .filter(f -> !filtersWithWrongEquipmentIds.containsKey(f.getId()) && exportFilters.containsKey(f.getId()))
                .flatMap(f -> exportFilters.get(f.getId())
                        .getIdentifiableAttributes()
                        .stream())
                .toList();
    }

    @Nullable
    public static Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(FilterService filterService, Network network, ReportNode subReportNode, Map<UUID, String> filters, NetworkModificationException.Type errorType) {
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReportNode, errorType, exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    public static Map<UUID, FilterEquipments> getUuidFilterWrongEquipmentsIdsMap(ReportNode subReportNode, Map<UUID, FilterEquipments> exportFilters, Map<UUID, String> filters) {
        // collect all filters with wrong equipments ids
        Map<UUID, FilterEquipments> filterWithWrongEquipmentsIds = exportFilters.entrySet().stream()
                .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        // create report for each wrong filter
        filterWithWrongEquipmentsIds.values().forEach(f -> {
            var equipmentIds = String.join(", ", f.getNotFoundEquipments());
            createReport(subReportNode,
                    "filterEquipmentsNotFound_" + f.getFilterName(),
                    "Cannot find the following equipments ${equipmentIds} in filter ${filters}",
                    Map.of("equipmentIds", equipmentIds, "filters", filters.get(f.getFilterId())), TypedValue.WARN_SEVERITY);
        });
        return filterWithWrongEquipmentsIds;
    }

    public static void insertReportNode(ReportNode parent, ReportNode child) {
        ReportNodeAdder adder = parent.newReportNode().withMessageTemplate(child.getMessageKey(), child.getMessageTemplate());
        for (Map.Entry<String, TypedValue> valueEntry : child.getValues().entrySet()) {
            adder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
        }
        TypedValue severity = child.getValue(ReportConstants.SEVERITY_KEY).orElse(null);
        if (severity != null) {
            adder.withSeverity(severity);
        }
        ReportNode insertedChild = adder.add();
        if (child.getChildren() != null) {
            child.getChildren().forEach(grandChild -> insertReportNode(insertedChild, grandChild));
        }
    }
}

