/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateCouplingDeviceBuilder;
import com.powsybl.iidm.modification.topology.CreateVoltageLevelTopologyBuilder;
import com.powsybl.iidm.modification.topology.TopologyModificationUtils;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
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

    private boolean checkBbs(Network network, String busbarSectionId1, String busbarSectionId2, Reporter subReporter) {
        Identifiable<?> busOrBbs1 = network.getIdentifiable(busbarSectionId1);
        Identifiable<?> busOrBbs2 = network.getIdentifiable(busbarSectionId2);
        if (busOrBbs1 == null) {
            subReporter.report(Report.builder()
                    .withKey("notFoundBurOrBusbarSection")
                    .withDefaultMessage("Bus or busbar section ID ${busbarSectionId} not found. Coupler was not created.")
                    .withValue(BUS_BAR_SECTION_ID, busbarSectionId1)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            return false;
        }
        if (busOrBbs2 == null) {
            subReporter.report(Report.builder()
                    .withKey("notFoundBurOrBusbarSection")
                    .withDefaultMessage("Bus or busbar section ID ${busbarSectionId} not found. Coupler was not created.")
                    .withValue(BUS_BAR_SECTION_ID, busbarSectionId2)
                    .withSeverity(TypedValue.ERROR_SEVERITY)
                    .build());
            return false;
        }
        return true;
    }

    void createVoltageLevel(VoltageLevelCreationInfos voltageLevelCreationInfos,
                                   Reporter subReporter, Network network) {
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
            if (!checkBbs(network, couplingDevice.getBusbarSectionId1(), couplingDevice.getBusbarSectionId2(), subReporter)) {
                return;
            }
            CreateCouplingDeviceBuilder couplingDeviceBuilder = new CreateCouplingDeviceBuilder();
            couplingDeviceBuilder.withBusOrBusbarSectionId1(couplingDevice.getBusbarSectionId1())
                .withBusOrBusbarSectionId2(couplingDevice.getBusbarSectionId2())
                .withSwitchPrefixId(voltageLevelCreationInfos.getEquipmentId() + "_COUPL")
                    .build().apply(network, subReporter);
        });

        subReporter.report(Report.builder()
            .withKey("voltageLevelCreated")
            .withDefaultMessage("New voltage level with id=${id} created")
            .withValue("id", voltageLevelCreationInfos.getEquipmentId())
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
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

    public static void createReport(Reporter reporter, String reporterKey, String message, TypedValue errorSeverity) {
        reporter.report(Report.builder()
                .withKey(reporterKey)
                .withDefaultMessage(message)
                .withSeverity(errorSeverity)
                .build());
    }

    public static <T> Predicate<T> distinctByKey(
            Function<? super T, ?> keyExtractor) {

        Map<Object, Boolean> seen = new ConcurrentHashMap<>();
        return t -> seen.putIfAbsent(keyExtractor.apply(t), Boolean.TRUE) == null;
    }

    public <T> Report applyElementaryModificationsAndReturnReport(Consumer<T> setter, Supplier<T> getter,
                                                                  AttributeModification<T> modification, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            return buildModificationReport(oldValue, newValue, fieldName);
        }
        return null;
    }

    public Report createEnabledDisabledReport(String key, boolean enabled) {
        return Report.builder().withKey(key)
                .withDefaultMessage(enabled ? "    Enabled" : "    Disables")
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public Reporter reportModifications(Reporter subReporter, List<Report> reports, String subReporterKey,
                                        String subReporterDefaultMessage) {
        List<Report> validReports = reports.stream().filter(Objects::nonNull).toList();
        Reporter modificationSubreporter = null;
        if (!validReports.isEmpty() && subReporter != null) {
            modificationSubreporter = subReporter.createSubReporter(subReporterKey, subReporterDefaultMessage);
            modificationSubreporter.report(Report.builder()
                    .withKey(subReporterKey)
                    .withDefaultMessage(subReporterDefaultMessage)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            validReports.stream().forEach(modificationSubreporter::report);
        }
        return modificationSubreporter;
    }

    public <T> void applyElementaryModifications(Consumer<T> setter, Supplier<T> getter,
            AttributeModification<T> modification,
            Reporter subReporter, String fieldName) {
        if (modification != null) {
            T oldValue = getter.get();
            T newValue = modification.applyModification(oldValue);
            setter.accept(newValue);

            subReporter.report(buildModificationReport(oldValue, newValue, fieldName));
        }
    }

    public <T> Report buildModificationReport(T oldValue, T newValue, String fieldName) {
        return buildModificationReport(oldValue, newValue, fieldName, 1, TypedValue.INFO_SEVERITY);
    }

    //TODO rename to buildModificationReport()
    public <T> Report buildModificationReportWithIndentation(T oldValue, T newValue, String fieldName, int indentationLevel) {
        return buildModificationReport(oldValue, newValue, fieldName, indentationLevel, TypedValue.INFO_SEVERITY);
    }

    static <T> Report buildModificationReport(T oldValue, T newValue, String fieldName, int indentationLevel, TypedValue severity) {
        final String oldValueString = (oldValue == null || oldValue instanceof Double oldDouble && Double.isNaN(oldDouble))
                ? NO_VALUE : oldValue.toString();
        final String newValueString = (newValue == null || newValue instanceof Double newDouble && Double.isNaN(newDouble))
                ? NO_VALUE : newValue.toString();
        final String indentation = "\t".repeat(indentationLevel);
        return Report.builder()
                .withKey("modification-indent" + indentationLevel)
                .withDefaultMessage(indentation + "${fieldName} : ${oldValue} → ${newValue}")
                .withValue("fieldName", fieldName)
                .withValue("oldValue", oldValueString)
                .withValue("newValue", newValueString)
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

    public void disconnectCreatedInjection(InjectionCreationInfos modificationInfos, Injection<?> injection, Reporter subReporter) {
        // A newly created injection is connected by default, unless we choose not to do
        if (!modificationInfos.isConnected()) {
            injection.getTerminal().disconnect();
            subReporter.report(Report.builder()
                    .withKey("equipmentDisconnected")
                    .withDefaultMessage("Equipment with id=${id} disconnected")
                    .withValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
    }

    public void modifyInjectionConnection(InjectionModificationInfos modificationInfos, Injection<?> injection) {
        if (modificationInfos.getConnected() != null) {
            if (injection.getTerminal().isConnected() && Boolean.FALSE.equals(modificationInfos.getConnected().getValue())) {
                injection.getTerminal().disconnect();
                if (injection.getTerminal().isConnected()) {
                    throw new NetworkModificationException(INJECTION_MODIFICATION_ERROR,
                        String.format("Could not disconnect equipment '%s'", injection.getId()));
                }
            } else if (!injection.getTerminal().isConnected() && Boolean.TRUE.equals(modificationInfos.getConnected().getValue())) {
                injection.getTerminal().connect();
                if (!injection.getTerminal().isConnected()) {
                    throw new NetworkModificationException(INJECTION_MODIFICATION_ERROR,
                        String.format("Could not connect equipment '%s'", injection.getId()));
                }
            }
        }
    }

    public void disconnectBranch(BranchCreationInfos modificationInfos, Branch<?> branch, Reporter subReporter) {
        // A newly created branch is connected by default on both sides, unless we choose not to do
        if (!modificationInfos.isConnected1()) {
            branch.getTerminal1().disconnect();
            subReporter.report(Report.builder()
                    .withKey("terminal1Disconnected")
                    .withDefaultMessage("Equipment with id=${id} disconnected on side 1")
                    .withValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        if (!modificationInfos.isConnected2()) {
            branch.getTerminal2().disconnect();
            subReporter.report(Report.builder()
                    .withKey("terminal2Disconnected")
                    .withDefaultMessage("Equipment with id=${id} disconnected on side 2")
                    .withValue("id", modificationInfos.getEquipmentId())
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
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

    public <T> Report buildCreationReport(T value, String fieldName) {
        String newValueString = value == null ? NO_VALUE : value.toString();
        return Report.builder()
                .withKey("Creation" + fieldName)
                .withDefaultMessage("    ${fieldName} : ${value}")
                .withValue("fieldName", fieldName)
                .withValue("value", newValueString)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public <T> void reportElementaryCreation(Reporter subReporter, T value, String fieldName) {
        subReporter.report(buildCreationReport(value, fieldName));
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
                                                    Reporter subReporter, Reporter subReporterLimits) {
        List<Report> reports = new ArrayList<>();
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
        Reporter subReporterReactiveLimits = null;
        Reporter subReporterLimits2 = subReporterLimits;
        if (subReporterLimits == null && !reports.isEmpty()) {
            subReporterLimits2 = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits2.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        if (subReporterLimits2 != null && !reports.isEmpty()) {
            subReporterReactiveLimits = subReporterLimits2.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                    .withKey(REACTIVE_LIMITS)
                    .withDefaultMessage(REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        reportModifications(subReporterReactiveLimits, reports, "curveReactiveLimitsModified", "By diagram");
    }

    public void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                    ReactiveCapabilityCurveModificationInfos newPoint,
                                                    ReactiveCapabilityCurve.Point oldPoint,
                                                    List<Report> reports,
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

    public void addToReports(List<Report> reports, Double newValue, Double oldValue, String fieldName) {
        if (newValue != null) {
            reports.add(buildModificationReport(oldValue, newValue, fieldName));
        }
    }

    public void modifyMinMaxReactiveLimits(AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower, ReactiveLimitsHolder reactiveLimitsHolder,
                                           Reporter subReporter, Reporter subReporterLimits) {
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
                newMinMaxReactiveLimitsAdder, subReporter, subReporterLimits,
                minimumReactivePower,
                maximumReactivePower);
    }

    public void modifyMinMaxReactiveLimits(MinMaxReactiveLimits minMaxReactiveLimits, MinMaxReactiveLimitsAdder newMinMaxReactiveLimits,
                                            Reporter subReporter, Reporter subReporterLimits, AttributeModification<Double> minimumReactivePower, AttributeModification<Double> maximumReactivePower) {
        List<Report> reports = new ArrayList<>();

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
        Reporter subReporterReactiveLimits = null;
        Reporter subReporterLimits2 = subReporterLimits;
        if (subReporterLimits == null && !reports.isEmpty()) {
            subReporterLimits2 = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits2.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        if (subReporterLimits2 != null && !reports.isEmpty()) {
            subReporterReactiveLimits = subReporterLimits2.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                    .withKey(REACTIVE_LIMITS)
                    .withDefaultMessage(REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        reportModifications(subReporterReactiveLimits, reports, "minMaxReactiveLimitsModified", "By range");
    }

    private void modifyExistingActivePowerControl(ActivePowerControl<?> activePowerControl,
                                                  AttributeModification<Boolean> participateInfo,
                                                  AttributeModification<Float> droopInfo,
                                                  List<Report> reports) {
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
                                             List<Report> reports) {
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

    public Reporter modifyActivePowerControlAttributes(ActivePowerControl<?> activePowerControl,
                                                       ActivePowerControlAdder<?> activePowerControlAdder,
                                                       AttributeModification<Boolean> participateInfo,
                                                       AttributeModification<Float> droopInfo,
                                                       Reporter subReporter,
                                                       Reporter subReporterSetpoints) {
        List<Report> reports = new ArrayList<>();
        if (activePowerControl != null) {
            modifyExistingActivePowerControl(activePowerControl, participateInfo, droopInfo, reports);
        } else {
            createNewActivePowerControl(activePowerControlAdder, participateInfo, droopInfo, reports);
        }

        Reporter subReporterSetpoints2 = subReporterSetpoints;
        if (subReporterSetpoints == null && !reports.isEmpty()) {
            subReporterSetpoints2 = subReporter.createSubReporter(SETPOINTS, SETPOINTS);
            subReporterSetpoints2.report(Report.builder()
                    .withKey(SETPOINTS)
                    .withDefaultMessage(SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        reportModifications(subReporterSetpoints2, reports, "activePowerRegulationModified", "Active power regulation");
        return subReporterSetpoints2;
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

    public static void addToReports(List<Report> reports, Double newValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildCreationReport(newValue, fieldName));
        }
    }

    public void createReactiveLimits(ReactiveLimitsHolderInfos creationInfos,
                                            ReactiveLimitsHolder reactiveLimitsHolder,
                                            Reporter subReporter) {
        if (Boolean.TRUE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createReactiveCapabilityCurve(creationInfos, reactiveLimitsHolder, subReporter);
        } else if (Boolean.FALSE.equals(creationInfos.getReactiveCapabilityCurve())) {
            createMinMaxReactiveLimits(creationInfos, reactiveLimitsHolder, subReporter);
        }
    }

    public void createMinMaxReactiveLimits(ReactiveLimitsHolderInfos batteryCreationInfos,
                                                  ReactiveLimitsHolder reactiveLimitsHolder,
                                                  Reporter subReporter) {
        List<Report> minMaxReactiveLimitsReports = new ArrayList<>();
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

            Reporter subReporterReactiveLimits = subReporter.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);

            subReporterReactiveLimits.report(Report.builder()
                    .withKey(REACTIVE_LIMITS)
                    .withDefaultMessage(REACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "minMaxReactiveLimitsCreated", "By range");
        }
    }

    public void createReactiveCapabilityCurve(ReactiveLimitsHolderInfos creationInfos,
                                                     ReactiveLimitsHolder reactiveLimitsHolder,
                                                     Reporter subReporter) {
        List<Report> pointsReports = new ArrayList<>();
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
        Reporter subReporterReactiveLimits = subReporter.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
        subReporterReactiveLimits.report(Report.builder()
                .withKey(REACTIVE_LIMITS)
                .withDefaultMessage(REACTIVE_LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, pointsReports, "curveReactiveLimitsCreated", "By diagram");
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                           ReactiveCapabilityCurveCreationInfos point,
                                                           List<Report> reports,
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

    public boolean isValidFilter(Reporter subReporter,
                                 NetworkModificationException.Type errorType,
                                 Map<UUID, FilterEquipments> exportFilters) {
        boolean noValidEquipmentId = exportFilters.values().stream()
                .allMatch(filterEquipments -> CollectionUtils.isEmpty(filterEquipments.getIdentifiableAttributes()));

        if (noValidEquipmentId) {
            String errorMsg = errorType + ": There is no valid equipment ID among the provided filter(s)";
            createReport(subReporter, "invalidFilters", errorMsg, TypedValue.ERROR_SEVERITY);
            return false;
        }

        return true;
    }

    public static List<IdentifiableAttributes> getIdentifiableAttributes(Map<UUID, FilterEquipments> exportFilters, Map<UUID, FilterEquipments> filtersWithWrongEquipmentIds, List<FilterInfos> filterInfos, Reporter subReporter) {
        filterInfos.stream()
                .filter(f -> !exportFilters.containsKey(f.getId()))
                .forEach(f -> createReport(subReporter,
                        "filterNotFound",
                        String.format("Cannot find the following filter: %s", f.getName()),
                        TypedValue.WARN_SEVERITY));

        return filterInfos
                .stream()
                .filter(f -> !filtersWithWrongEquipmentIds.containsKey(f.getId()) && exportFilters.containsKey(f.getId()))
                .flatMap(f -> exportFilters.get(f.getId())
                        .getIdentifiableAttributes()
                        .stream())
                .toList();
    }

    @Nullable
    public static Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(FilterService filterService, Network network, Reporter subReporter, Map<UUID, String> filters, NetworkModificationException.Type errorType) {
        Map<UUID, FilterEquipments> exportFilters = filterService.getUuidFilterEquipmentsMap(network, filters);

        boolean isValidFilter = ModificationUtils.getInstance().isValidFilter(subReporter, errorType, exportFilters);
        return isValidFilter ? exportFilters : null;
    }

    public static Map<UUID, FilterEquipments> getUuidFilterWrongEquipmentsIdsMap(Reporter subReporter, Map<UUID, FilterEquipments> exportFilters, Map<UUID, String> filters) {
        // collect all filters with wrong equipments ids
        Map<UUID, FilterEquipments> filterWithWrongEquipmentsIds = exportFilters.entrySet().stream()
                .filter(e -> !CollectionUtils.isEmpty(e.getValue().getNotFoundEquipments()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        // create report for each wrong filter
        filterWithWrongEquipmentsIds.values().forEach(f -> {
            var equipmentIds = String.join(", ", f.getNotFoundEquipments());
            createReport(subReporter,
                    "filterEquipmentsNotFound_" + f.getFilterName(),
                    String.format("Cannot find the following equipments %s in filter %s", equipmentIds, filters.get(f.getFilterId())),
                    TypedValue.WARN_SEVERITY);
        });
        return filterWithWrongEquipmentsIds;
    }
}

