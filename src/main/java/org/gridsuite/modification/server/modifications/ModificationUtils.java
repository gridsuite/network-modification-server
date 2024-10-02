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
    public static final String NOT_EXIST_IN_NETWORK = " does not exist in network";

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
            throw new NetworkModificationException(BATTERY_NOT_FOUND, "Battery " + batteryId + NOT_EXIST_IN_NETWORK);
        }
        return battery;
    }

    Generator getGenerator(Network network, String generatorId) {
        Generator generator = network.getGenerator(generatorId);
        if (generator == null) {
            throw new NetworkModificationException(GENERATOR_NOT_FOUND, "Generator " + generatorId + NOT_EXIST_IN_NETWORK);
        }
        return generator;
    }

    VscConverterStation getVscConverterStation(Network network, String converterStationId) {
        VscConverterStation vscConverterStation = network.getVscConverterStation(converterStationId);
        if (vscConverterStation == null) {
            throw new NetworkModificationException(VSC_CONVERTER_STATION_NOT_FOUND, "Vsc converter station  " + converterStationId + NOT_EXIST_IN_NETWORK);
        }
        return vscConverterStation;
    }

    //get hvdcline
    HvdcLine getHvdcLine(Network network, String hvdcLineId) {
        HvdcLine hvdcLine = network.getHvdcLine(hvdcLineId);
        if (hvdcLine == null) {
            throw new NetworkModificationException(HVDC_LINE_NOT_FOUND, "Hvdc line  " + hvdcLineId + NOT_EXIST_IN_NETWORK);
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

            return buildModificationReport(oldValue, newValue, fieldName, indentationLevel);
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

    public ReportNode reportModifications(ReportNode reportNode, List<ReportNode> reports, String subReportNodeKey, String subReportNodeMessage) {
        List<ReportNode> validReports = reports.stream().filter(Objects::nonNull).toList();
        ReportNode subReportNode = null;
        if (!validReports.isEmpty() && reportNode != null) {
            // new child report node
            subReportNode = reportNode.newReportNode().withMessageTemplate(subReportNodeKey, subReportNodeMessage).add();
            for (ReportNode report : validReports) {
                ReportNodeAdder reportNodeAdder = subReportNode.newReportNode().withMessageTemplate(report.getMessageKey(), report.getMessageTemplate()).withSeverity(TypedValue.INFO_SEVERITY);
                for (Map.Entry<String, TypedValue> valueEntry : report.getValues().entrySet()) {
                    reportNodeAdder.withUntypedValue(valueEntry.getKey(), valueEntry.getValue().toString());
                }
                report.getValue(ReportConstants.SEVERITY_KEY).ifPresent(reportNodeAdder::withSeverity);
                reportNodeAdder.add();
            }
        }
        return subReportNode;
    }

    public <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
            AttributeModification<T> modification,
            ReportNode subReportNode, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            if (subReportNode != null) {
                insertReportNode(subReportNode, buildModificationReport(oldValue, newValue, fieldName));
            }
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

    public <T> ReportNode buildModificationReport(T oldValue, T newValue, String fieldName, int indentationLevel) {
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
        if (isVoltageOrBusbarIdMissing(modificationInfos.getVoltageLevelId(), modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getEquipmentId(), reports)) {
            return reports.get(0);
        }
        processConnectivityPosition(connectablePosition, connectablePositionAdder, modificationInfos, injection.getNetwork(), reports, false);
        modifyConnection(modificationInfos.getTerminalConnected(), injection, injection.getTerminal(), reports);

        return reportModifications(connectivityReports, reports, "ConnectivityModified", CONNECTIVITY);
    }

    public ReportNode modifyBranchConnectivityAttributes(ConnectablePosition<?> connectablePosition,
                                                         ConnectablePositionAdder<?> connectablePositionAdder,
                                                         Branch<?> branch,
                                                         BranchModificationInfos modificationInfos,
                                                         ReportNode connectivityReports) {
        List<ReportNode> reports = new ArrayList<>();
        if (isVoltageOrBusbarIdMissing(modificationInfos.getVoltageLevelId1(), modificationInfos.getBusOrBusbarSectionId1(), modificationInfos.getEquipmentId(), reports) ||
            isVoltageOrBusbarIdMissing(modificationInfos.getVoltageLevelId2(), modificationInfos.getBusOrBusbarSectionId2(), modificationInfos.getEquipmentId(), reports)) {
            return reports.get(0);
        }

        processConnectivityPosition(connectablePosition, connectablePositionAdder, modificationInfos, branch.getNetwork(), reports, true);
        modifyConnection(modificationInfos.getTerminal1Connected(), branch, branch.getTerminal1(), reports);
        modifyConnection(modificationInfos.getTerminal2Connected(), branch, branch.getTerminal2(), reports);

        return reportModifications(connectivityReports, reports, "ConnectivityModified", CONNECTIVITY);
    }

    private boolean isVoltageOrBusbarIdMissing(AttributeModification<String> voltageLevelId, AttributeModification<String> busbarSectionId, String equipmentId, List<ReportNode> reports) {
        if (voltageLevelId == null || busbarSectionId == null) {
            reports.add(ReportNode.newRootReportNode()
                    .withMessageTemplate("VoltageLevelOrBusbarSectionNotFound",
                            "Voltage level id or Bus bar section id of equipment id=${id} not found")
                    .withUntypedValue("id", equipmentId)
                    .withSeverity(TypedValue.WARN_SEVERITY)
                    .build());
            return true;
        }
        return false;
    }

    private void processConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                             ConnectablePositionAdder<?> connectablePositionAdder,
                                             BasicEquipmentModificationInfos modificationInfos,
                                             Network network,
                                             List<ReportNode> reports,
                                             boolean isBranch) {
        if (connectablePosition != null) {
            modifyExistingConnectivityPosition(connectablePosition, modificationInfos, reports, isBranch);
        } else {
            createNewConnectivityPosition(connectablePositionAdder, modificationInfos, network, reports, isBranch);
        }
    }

    private void modifyExistingConnectivityPosition(ConnectablePosition<?> connectablePosition,
                                                    BasicEquipmentModificationInfos modificationInfos,
                                                    List<ReportNode> reports,
                                                    boolean isBranch) {
        if (isBranch) {
            modifyFeeder(connectablePosition.getFeeder1(), modificationInfos, reports, 1);
            modifyFeeder(connectablePosition.getFeeder2(), modificationInfos, reports, 2);
        } else {
            modifyFeeder(connectablePosition.getFeeder(), modificationInfos, reports, 0);
        }
    }

    private void createNewConnectivityPosition(ConnectablePositionAdder<?> adder,
                                               BasicEquipmentModificationInfos modificationInfos,
                                               Network network,
                                               List<ReportNode> reports,
                                               boolean isBranch) {
        if (isBranch) {
            ConnectablePositionAdder.FeederAdder<?> feeder1 = adder.newFeeder1();
            ConnectablePositionAdder.FeederAdder<?> feeder2 = adder.newFeeder2();
            modifyFeederAdder(feeder1, modificationInfos, network, reports, 1);
            modifyFeederAdder(feeder2, modificationInfos, network, reports, 2);
        } else {
            ConnectablePositionAdder.FeederAdder<?> feeder = adder.newFeeder();
            modifyFeederAdder(feeder, modificationInfos, network, reports, 0);
        }
    }

    private void modifyFeeder(ConnectablePosition.Feeder feeder,
                              BasicEquipmentModificationInfos modificationInfos,
                              List<ReportNode> reports,
                              int feederNumber) {
        applyModifications(feeder, modificationInfos, reports, feederNumber);
    }

    private void applyModifications(ConnectablePosition.Feeder feeder,
                                    BasicEquipmentModificationInfos modificationInfos,
                                    List<ReportNode> reports,
                                    int feederNumber) {
        ReportNode connectionNameReport = applyElementaryModificationsAndReturnReport(feeder::setName,
                feeder.getName()::get,
                getConnectionName(modificationInfos, feederNumber),
                getConnectionNameField(feederNumber));
        if (connectionNameReport != null) {
            reports.add(connectionNameReport);
        }
        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(feeder::setDirection,
                feeder::getDirection,
                getConnectionDirection(modificationInfos, feederNumber),
                getConnectionDirectionField(feederNumber));
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }
        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(feeder::setOrder,
                feeder.getOrder()::get,
                getConnectionPosition(modificationInfos, feederNumber),
                getConnectionPositionField(feederNumber));
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }
    }

    private void modifyFeederAdder(ConnectablePositionAdder.FeederAdder<?> feeder,
                                   BasicEquipmentModificationInfos modificationInfos,
                                   Network network,
                                   List<ReportNode> reports,
                                   int feederNumber) {
        AttributeModification<String> connectionName = getConnectionName(modificationInfos, feederNumber);
        AttributeModification<ConnectablePosition.Direction> connectionDirection = getConnectionDirection(modificationInfos, feederNumber);
        AttributeModification<Integer> connectionPosition = getConnectionPosition(modificationInfos, feederNumber);
        AttributeModification<String> equipmentId = getEquipmentId(modificationInfos);
        AttributeModification<String> voltageLevelId = getVoltageLevelId(modificationInfos, feederNumber);
        AttributeModification<String> busOrBusbarSectionId = getBusOrBusbarSectionId(modificationInfos, feederNumber);
        int position = getPosition(connectionPosition == null ? null : connectionPosition.getValue(),
                busOrBusbarSectionId == null ? null : busOrBusbarSectionId.getValue(),
                network,
                getVoltageLevel(network, voltageLevelId == null ? null : voltageLevelId.getValue()));
        ReportNode connectionNameReport = applyElementaryModificationsAndReturnReport(feeder::withName,
                () -> null,
                connectionName == null && (connectionDirection != null || connectionPosition != null) ? equipmentId : connectionName,
                getConnectionNameField(feederNumber));
        if (connectionNameReport != null) {
            reports.add(connectionNameReport);
        }
        ReportNode connectionDirectionReport = applyElementaryModificationsAndReturnReport(feeder::withDirection,
                () -> null,
                connectionDirection == null && (connectionName != null || connectionPosition != null) ?
                        new AttributeModification<>(ConnectablePosition.Direction.UNDEFINED, OperationType.SET) : connectionDirection,
                getConnectionDirectionField(feederNumber));
        if (connectionDirectionReport != null) {
            reports.add(connectionDirectionReport);
        }
        ReportNode connectionPositionReport = applyElementaryModificationsAndReturnReport(feeder::withOrder,
                () -> null,
                connectionPosition == null && (connectionName != null || connectionDirection != null) ?
                        new AttributeModification<>(position, OperationType.SET) : connectionPosition,
                getConnectionPositionField(feederNumber));
        if (connectionPositionReport != null) {
            reports.add(connectionPositionReport);
        }
        if (connectionNameReport != null || connectionDirectionReport != null || connectionPositionReport != null) {
            feeder.add();
        }
    }

    private <T> T getConnectionDetail(BasicEquipmentModificationInfos modificationInfos, int feederNumber,
                                      Function<BranchModificationInfos, T> branchFunc1,
                                      Function<BranchModificationInfos, T> branchFunc2,
                                      Function<InjectionModificationInfos, T> injectionFunc) {
        if (modificationInfos instanceof BranchModificationInfos branchInfo) {
            return feederNumber == 1 ? branchFunc1.apply(branchInfo) : branchFunc2.apply(branchInfo);
        } else if (modificationInfos instanceof InjectionModificationInfos injectionInfo) {
            return injectionFunc.apply(injectionInfo);
        }
        return null;
    }

    private String getConnectionFieldName(int feederNumber, String baseFieldName) {
        return switch (feederNumber) {
            case 0 -> baseFieldName;
            case 1 -> baseFieldName + " 1";
            case 2 -> baseFieldName + " 2";
            default -> "";
        };
    }

    private AttributeModification<String> getVoltageLevelId(BasicEquipmentModificationInfos modificationInfos, int feederNumber) {
        return getConnectionDetail(modificationInfos, feederNumber,
                BranchModificationInfos::getVoltageLevelId1, BranchModificationInfos::getVoltageLevelId2,
                InjectionModificationInfos::getVoltageLevelId);
    }

    private AttributeModification<String> getBusOrBusbarSectionId(BasicEquipmentModificationInfos modificationInfos, int feederNumber) {
        return getConnectionDetail(modificationInfos, feederNumber,
                BranchModificationInfos::getBusOrBusbarSectionId1, BranchModificationInfos::getBusOrBusbarSectionId2,
                InjectionModificationInfos::getBusOrBusbarSectionId);
    }

    private AttributeModification<String> getEquipmentId(BasicEquipmentModificationInfos modificationInfos) {
        if (modificationInfos instanceof BranchModificationInfos branchInfo) {
            return AttributeModification.toAttributeModification(branchInfo.getEquipmentId(), OperationType.SET);
        } else if (modificationInfos instanceof InjectionModificationInfos injectionInfo) {
            return AttributeModification.toAttributeModification(injectionInfo.getEquipmentId(), OperationType.SET);
        }
        return null;
    }

    private AttributeModification<String> getConnectionName(BasicEquipmentModificationInfos modificationInfos, int feederNumber) {
        return getConnectionDetail(modificationInfos, feederNumber,
                BranchModificationInfos::getConnectionName1, BranchModificationInfos::getConnectionName2,
                InjectionModificationInfos::getConnectionName);
    }

    private String getConnectionNameField(int feederNumber) {
        return getConnectionFieldName(feederNumber, CONNECTION_NAME_FIELD_NAME);
    }

    private String getConnectionDirectionField(int feederNumber) {
        return getConnectionFieldName(feederNumber, CONNECTION_DIRECTION_FIELD_NAME);
    }

    private String getConnectionPositionField(int feederNumber) {
        return getConnectionFieldName(feederNumber, CONNECTION_POSITION_FIELD_NAME);
    }

    private AttributeModification<ConnectablePosition.Direction> getConnectionDirection(BasicEquipmentModificationInfos modificationInfos, int feederNumber) {
        return getConnectionDetail(modificationInfos, feederNumber,
                BranchModificationInfos::getConnectionDirection1, BranchModificationInfos::getConnectionDirection2,
                InjectionModificationInfos::getConnectionDirection);
    }

    private AttributeModification<Integer> getConnectionPosition(BasicEquipmentModificationInfos modificationInfos, int feederNumber) {
        return getConnectionDetail(modificationInfos, feederNumber,
                BranchModificationInfos::getConnectionPosition1, BranchModificationInfos::getConnectionPosition2,
                InjectionModificationInfos::getConnectionPosition);
    }

    private void modifyConnection(AttributeModification<Boolean> terminalConnected, Identifiable<?> equipment, Terminal terminal, List<ReportNode> reports) {
        if (terminalConnected == null || equipment == null) {
            return;
        }

        boolean isConnected = terminal.isConnected();
        if (isConnected && Boolean.FALSE.equals(terminalConnected.getValue())) {
            terminal.disconnect();
            validateConnectionChange(!terminal.isConnected(), equipment, "disconnect", reports);
        } else if (!isConnected && Boolean.TRUE.equals(terminalConnected.getValue())) {
            terminal.connect();
            validateConnectionChange(terminal.isConnected(), equipment, "connect", reports);
        }
    }

    private void validateConnectionChange(boolean success, Identifiable<?> equipment, String action, List<ReportNode> reports) {
        if (!success) {
            throw new NetworkModificationException(equipment instanceof Branch<?> ? BRANCH_MODIFICATION_ERROR : INJECTION_MODIFICATION_ERROR,
                    String.format("Could not %s equipment '%s'", action, equipment.getId()));
        }
        reports.add(ReportNode.newRootReportNode()
                .withMessageTemplate("equipment" + capitalize(action), String.format("Equipment with id=${id} %sed", action))
                .withUntypedValue("id", equipment.getId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private String capitalize(String input) {
        return input.substring(0, 1).toUpperCase() + input.substring(1);
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

        return switch (type) {
            case HVDC_LINE -> network.getHvdcLine(equipmentId);
            case LINE -> network.getLine(equipmentId);
            case TWO_WINDINGS_TRANSFORMER -> network.getTwoWindingsTransformer(equipmentId);
            case THREE_WINDINGS_TRANSFORMER -> network.getThreeWindingsTransformer(equipmentId);
            case GENERATOR -> network.getGenerator(equipmentId);
            case LOAD -> network.getLoad(equipmentId);
            case BATTERY -> network.getBattery(equipmentId);
            case SHUNT_COMPENSATOR -> network.getShuntCompensator(equipmentId);
            case STATIC_VAR_COMPENSATOR -> network.getStaticVarCompensator(equipmentId);
            case DANGLING_LINE -> network.getDanglingLine(equipmentId);
            case HVDC_CONVERTER_STATION -> network.getHvdcConverterStation(equipmentId);
            case SUBSTATION -> network.getSubstation(equipmentId);
            case VOLTAGE_LEVEL -> network.getVoltageLevel(equipmentId);
            case BUSBAR_SECTION -> network.getBusbarSection(equipmentId);
            default -> null;
        };
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
        if (!reports.isEmpty()) {
            if (subReportNodeLimits == null) {
                subReporterLimits2 = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
            }
            if (subReporterLimits2 != null) {
                subReportNodeReactiveLimits = subReporterLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
            }
        }
        reportModifications(subReportNodeReactiveLimits, reports, "curveReactiveLimitsModified", "By diagram");
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
        }
        if (subReportNodeLimits2 != null && !reports.isEmpty()) {
            subReportNodeReactiveLimits = subReportNodeLimits2.newReportNode().withMessageTemplate(REACTIVE_LIMITS, REACTIVE_LIMITS).add();
        }
        reportModifications(subReportNodeReactiveLimits, reports, "minMaxReactiveLimitsModified", "By range");
    }

    private void modifyExistingActivePowerControl(ActivePowerControl<?> activePowerControl,
                                                  AttributeModification<Boolean> participateInfo,
                                                  AttributeModification<Float> droopInfo,
                                                  List<ReportNode> reports) {
        double oldDroop = activePowerControl.getDroop();
        boolean oldParticipate = activePowerControl.isParticipate();

        Optional.ofNullable(participateInfo).ifPresent(info -> {
            activePowerControl.setParticipate(info.getValue());
            if (reports != null) {
                reports.add(buildModificationReport(oldParticipate, info.getValue(), "Participate"));
            }
        });

        Optional.ofNullable(droopInfo).ifPresent(info -> {
            activePowerControl.setDroop(info.getValue());
            if (reports != null) {
                reports.add(buildModificationReport(oldDroop, info.getValue(), "Droop"));
            }
        });
    }

    private void createNewActivePowerControl(ActivePowerControlAdder<?> adder,
                                             AttributeModification<Boolean> participateInfo,
                                             AttributeModification<Float> droopInfo,
                                             List<ReportNode> reports) {
        boolean participate = participateInfo != null ? participateInfo.getValue() : false;
        adder.withParticipate(participate);
        if (participateInfo != null && reports != null) {
            reports.add(buildModificationReport(null, participate, "Participate"));
        }
        double droop = droopInfo != null ? droopInfo.getValue() : Double.NaN;
        adder.withDroop(droop);
        if (droopInfo != null && reports != null) {
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
        if (subReportNode != null) {
            ReportNode subReportNodeSetpoints2 = subReporterSetpoints;
            if (subReporterSetpoints == null && !reports.isEmpty()) {
                subReportNodeSetpoints2 = subReportNode.newReportNode().withMessageTemplate(SETPOINTS, SETPOINTS).add();
            }
            reportModifications(subReportNodeSetpoints2, reports, "activePowerRegulationModified", "Active power regulation");
            return subReportNodeSetpoints2;
        }
        return null;
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

            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "minMaxReactiveLimitsCreated", "By range");
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
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, pointsReports, "curveReactiveLimitsCreated", "By diagram");
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
        child.getValue(ReportConstants.SEVERITY_KEY).ifPresent(adder::withSeverity);
        ReportNode insertedChild = adder.add();
        if (child.getChildren() != null) {
            child.getChildren().forEach(grandChild -> insertReportNode(insertedChild, grandChild));
        }
    }
}

