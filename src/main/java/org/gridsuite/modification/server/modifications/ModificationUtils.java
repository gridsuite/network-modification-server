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

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.IntStream;

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
        if (Objects.nonNull(voltageLevelCreationInfos.getIpMin()) && Objects.isNull(voltageLevelCreationInfos.getIpMax())) {
            throw new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required");
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
            .setNominalV(voltageLevelCreationInfos.getNominalVoltage())
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
                .setR(lineCreationInfos.getSeriesResistance())
                .setX(lineCreationInfos.getSeriesReactance())
                .setG1(lineCreationInfos.getShuntConductance1() != null ? lineCreationInfos.getShuntConductance1() : 0.0)
                .setB1(lineCreationInfos.getShuntSusceptance1() != null ? lineCreationInfos.getShuntSusceptance1() : 0.0)
                .setG2(lineCreationInfos.getShuntConductance2() != null ? lineCreationInfos.getShuntConductance2() : 0.0)
                .setB2(lineCreationInfos.getShuntSusceptance2() != null ? lineCreationInfos.getShuntSusceptance2() : 0.0);

        // lineAdder completion by topology
        setBranchAdderNodeOrBus(lineAdder, voltageLevel1, lineCreationInfos, Branch.Side.ONE, withSwitch1);
        setBranchAdderNodeOrBus(lineAdder, voltageLevel2, lineCreationInfos, Branch.Side.TWO, withSwitch2);

        return lineAdder;
    }

    void setBranchAdderNodeOrBus(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, BranchCreationInfos branchCreationInfos,
                                 Branch.Side side, boolean withSwitch) {
        String busOrBusbarSectionId = (side == Branch.Side.ONE) ? branchCreationInfos.getBusOrBusbarSectionId1() : branchCreationInfos.getBusOrBusbarSectionId2();
        if (voltageLevel.getTopologyKind() == TopologyKind.BUS_BREAKER) {
            setBranchAdderBusBreaker(branchAdder, voltageLevel, side, busOrBusbarSectionId);
        } else {
            if (withSwitch) { // NODE_BREAKER
                setBranchAdderNodeBreaker(branchAdder, voltageLevel, branchCreationInfos, side, busOrBusbarSectionId);
            }
        }
    }

    private void setBranchAdderBusBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel, Branch.Side side, String busId) {
        Bus bus = getBusBreakerBus(voltageLevel, busId);

        // complete the lineAdder
        if (side == Branch.Side.ONE) {
            branchAdder.setBus1(bus.getId()).setConnectableBus1(bus.getId());
        } else {
            branchAdder.setBus2(bus.getId()).setConnectableBus2(bus.getId());
        }
    }

    private void setBranchAdderNodeBreaker(BranchAdder<?, ?> branchAdder, VoltageLevel voltageLevel,
                                           BranchCreationInfos branchCreationInfos, Branch.Side side,
                                           String currentBusBarSectionId) {
        // create cell switches
        String sideSuffix = side != null ? "_" + side.name() : "";
        int nodeNum = createNodeBreakerCellSwitches(voltageLevel,
            currentBusBarSectionId,
            branchCreationInfos.getEquipmentId(),
            branchCreationInfos.getEquipmentName(),
            sideSuffix);

        // complete the lineAdder
        if (side == Branch.Side.ONE) {
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
        return buildModificationReportWithIndentation(oldValue, newValue, fieldName, 1);
    }

    public <T> Report buildModificationReportWithIndentation(T oldValue, T newValue, String fieldName, int indentationLevel) {
        boolean isOldValueDoubleNaN = (oldValue instanceof Double) && Double.isNaN((Double) oldValue);
        String oldValueString = (oldValue == null || isOldValueDoubleNaN) ? NO_VALUE : oldValue.toString();
        boolean isNewValueDoubleNaN = (newValue instanceof Double) && Double.isNaN((Double) newValue);
        String newValueString = (newValue == null || isNewValueDoubleNaN) ? NO_VALUE : newValue.toString();
        StringBuilder indentation = new StringBuilder();
        for (int i = 0; i < indentationLevel; i++) {
            indentation.append("    ");
        }
        return Report.builder()
                .withKey("Modification" + fieldName)
                .withDefaultMessage(indentation.toString() + "${fieldName} : ${oldValue} -> ${newValue}")
                .withValue("fieldName", fieldName)
                .withValue("oldValue", oldValueString)
                .withValue("newValue", newValueString)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build();
    }

    public Terminal getTerminalFromIdentifiable(Network network,
            String equipmentId,
            String type,
            String voltageLevelId) {
        if (network != null && equipmentId != null && type != null && voltageLevelId != null) {
            Identifiable<?> identifiable = getEquipmentByIdentifiableType(network, type, equipmentId);

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

    public Identifiable<?> getEquipmentByIdentifiableType(Network network, String type, String equipmentId) {
        if (type == null || equipmentId == null) {
            return null;
        }

        switch (IdentifiableType.valueOf(type)) {
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
        var maxQ = newPoint.getQmaxP() != null ? newPoint.getQmaxP() : oldMaxQ;
        var minQ = newPoint.getQminP() != null ? newPoint.getQminP() : oldMinQ;
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

    public Reporter modifyActivePowerControlAttributes(ActivePowerControl<?> activePowerControl,
                                                       ActivePowerControlAdder<?> activePowerControlAdder,
                                                       AttributeModification<Boolean> participateInfo,
                                                       AttributeModification<Float> droopInfo,
                                                       Reporter subReporter,
                                                       Reporter subReporterSetpoints) {
        List<Report> reports = new ArrayList<>();
        double oldDroop = Double.NaN;
        boolean oldParticipate = false;
        double droop = Double.NaN;
        if (activePowerControl != null) {
            oldDroop = activePowerControl.getDroop();
            oldParticipate = activePowerControl.isParticipate();
        }

        if (participateInfo != null) {
            activePowerControlAdder
                    .withParticipate(participateInfo.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(activePowerControl != null ? activePowerControl.isParticipate() : null,
                    oldParticipate,
                    "Participate"));
        } else {
            activePowerControlAdder
                    .withParticipate(oldParticipate);
        }

        if (droopInfo != null) {
            activePowerControlAdder
                    .withDroop(droopInfo.getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop,
                    droop,
                    "Droop"));
        } else {
            activePowerControlAdder
                    .withDroop(oldDroop);
        }
        activePowerControlAdder
                .add();

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

    public void checkMaxQGreaterThanMinQ(List<ReactiveCapabilityCurve.Point> equipmentPoints, List<ReactiveCapabilityCurveModificationInfos> modificationPoints,
                                         NetworkModificationException.Type exceptionType, String errorMessage) {
        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    ReactiveCapabilityCurve.Point oldPoint = equipmentPoints.get(i);
                    ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                    Double oldMaxQ = Double.NaN;
                    Double oldMinQ = Double.NaN;
                    if (oldPoint != null) {
                        oldMaxQ = oldPoint.getMaxQ();
                        oldMinQ = oldPoint.getMinQ();
                    }
                    var maxQ = newPoint.getQmaxP() != null ? newPoint.getQmaxP() : oldMaxQ;
                    var minQ = newPoint.getQminP() != null ? newPoint.getQminP() : oldMinQ;
                    if (maxQ < minQ) {
                        throw new NetworkModificationException(exceptionType, errorMessage + "maximum reactive power " + maxQ + " is expected to be greater than or equal to minimum reactive power " + minQ);
                    }
                });
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
}

