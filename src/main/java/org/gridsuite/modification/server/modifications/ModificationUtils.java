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
import com.powsybl.iidm.network.extensions.BusbarSectionPosition;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;

import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.AttributeModification;

import javax.validation.constraints.NotNull;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
// TODO transfer to powsybl-core (com.powsybl.iidm.modification)
// TODO remove public qualifier for all methods
public final class ModificationUtils {

    public static final String DISCONNECTOR = "disconnector_";
    public static final String BREAKER = "breaker_";

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
            CreateCouplingDeviceBuilder couplingDeviceBuilder = new CreateCouplingDeviceBuilder();
            couplingDeviceBuilder.withBusOrBusbarSectionId1(couplingDevice.getBusbarSectionId1())
                .withBusOrBusbarSectionId2(couplingDevice.getBusbarSectionId2())
                .withSwitchPrefixId(voltageLevelCreationInfos.getEquipmentId() + "_COUPL")
                .build().apply(network);
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
        List<Report> validReports = reports.stream().filter(Objects::nonNull).collect(Collectors.toList());
        Reporter modificationSubreporter = null;
        if (!validReports.isEmpty()) {
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
        String oldValueString = oldValue == null ? "NaN" : oldValue.toString();
        String newValueString = newValue == null ? "NaN" : newValue.toString();
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
        String newValueString = value == null ? "NaN" : value.toString();
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
}

