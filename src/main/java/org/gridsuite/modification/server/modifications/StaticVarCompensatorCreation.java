/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.commons.report.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.gridsuite.modification.server.NetworkModificationException.Type.STATIC_VAR_COMPENSATOR_ALREADY_EXISTS;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class StaticVarCompensatorCreation extends AbstractModification {

    private final StaticVarCompensatorCreationInfos modificationInfos;
    private static final String CONNECTIVITY = "Connectivity";

    public StaticVarCompensatorCreation(StaticVarCompensatorCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getStaticVarCompensator(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(STATIC_VAR_COMPENSATOR_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }

        // check connectivity
        ModificationUtils.getInstance()
                .controlConnectivity(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());

        // check reactive power limits and set points
        ModificationUtils.getInstance().checkReactivePowerLimitsAndSetPointsCreation(modificationInfos.getMaxSusceptance(),
                modificationInfos.getMinSusceptance(), modificationInfos.getMinQAtNominalV(),
                modificationInfos.getMaxQAtNominalV(), modificationInfos.getVoltageSetpoint(),
                modificationInfos.getReactivePowerSetpoint(), modificationInfos.getRegulationMode(),
                modificationInfos.getErrorType(),
                modificationInfos.getEquipmentId(),
                "StaticVarCompensator");

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(), modificationInfos.getRegulatingTerminalId(),
                modificationInfos.getRegulatingTerminalType(), modificationInfos.getRegulatingTerminalVlId());

        // check standby automaton
        ModificationUtils.getInstance().checkStandByAutomateCreation(modificationInfos.isStandby(), modificationInfos.getB0(),
                modificationInfos.getQ0(), modificationInfos.getMinSusceptance(), modificationInfos.getMaxSusceptance(),
                modificationInfos.getMinQAtNominalV(), modificationInfos.getMaxQAtNominalV(),
                modificationInfos.getRegulationMode(), modificationInfos.getErrorType(), modificationInfos.getEquipmentId(),
                "StaticVarCompensator");
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the static var compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (Objects.isNull(modificationInfos.getMaxSusceptance()) && Objects.nonNull(modificationInfos.getMaxQAtNominalV())) {
            modificationInfos.setMaxSusceptance((modificationInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2));
        }
        if (Objects.isNull(modificationInfos.getMinSusceptance()) && Objects.nonNull(modificationInfos.getMinQAtNominalV())) {
            modificationInfos.setMinSusceptance((modificationInfos.getMinQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2));
        }
        if (Boolean.TRUE.equals(modificationInfos.isStandByAutomateOn()) && Objects.isNull(modificationInfos.getB0())
            && Objects.nonNull(modificationInfos.getQ0())) {
            modificationInfos.setB0((modificationInfos.getQ0()) / Math.pow(voltageLevel.getNominalV(), 2));
        }
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createStaticVarCompensatorInNodeBreaker(voltageLevel, modificationInfos, network, subReportNode);
        } else {
            createStaticVarCompensatorInBusBreaker(voltageLevel, modificationInfos, subReportNode);
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationInfos, network.getStaticVarCompensator(modificationInfos.getEquipmentId()), subReportNode);
        // properties
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, modificationInfos.getProperties(), "StaticVarCompensatorProperties");
    }

    private void createStaticVarCompensatorInNodeBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
            Network network, ReportNode subReportNode) {
        StaticVarCompensatorAdder staticVarCompensatorAdder = createStaticVarCompensatorAdderInNodeBreaker(voltageLevel, staticVarCompensatorCreationInfos);
        var position = ModificationUtils.getInstance().getPosition(staticVarCompensatorCreationInfos.getConnectionPosition(),
                staticVarCompensatorCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);
        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(staticVarCompensatorCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(staticVarCompensatorCreationInfos.getConnectionDirection())
                .withInjectionFeederName(staticVarCompensatorCreationInfos.getConnectionName() != null
                        ? staticVarCompensatorCreationInfos.getConnectionName()
                        : staticVarCompensatorCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(staticVarCompensatorAdder)
                .build();
        algo.apply(network, true, subReportNode);
        var staticVarCompensator = ModificationUtils.getInstance().staticVarCompensator(network, staticVarCompensatorCreationInfos.getEquipmentId());
        addExtensionsToStaticVarCompensator(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);
    }

    private StaticVarCompensatorAdder createStaticVarCompensatorAdderInNodeBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                staticVarCompensatorCreationInfos.getRegulatingTerminalId(),
                staticVarCompensatorCreationInfos.getRegulatingTerminalType(),
                staticVarCompensatorCreationInfos.getRegulatingTerminalVlId());

        StaticVarCompensatorAdder staticVarCompensatorAdder = voltageLevel.newStaticVarCompensator()
                .setId(staticVarCompensatorCreationInfos.getEquipmentId())
                .setName(staticVarCompensatorCreationInfos.getEquipmentName())
                .setBmax(staticVarCompensatorCreationInfos.getMaxSusceptance())
                .setBmin(staticVarCompensatorCreationInfos.getMinSusceptance())
                .setVoltageSetpoint(staticVarCompensatorCreationInfos.getVoltageSetpoint())
                .setReactivePowerSetpoint(staticVarCompensatorCreationInfos.getReactivePowerSetpoint())
                .setRegulationMode(staticVarCompensatorCreationInfos.getRegulationMode());

        if (terminal != null) {
            staticVarCompensatorAdder.setRegulatingTerminal(terminal);
        }

        return staticVarCompensatorAdder;
    }

    private void addExtensionsToStaticVarCompensator(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
                                                     StaticVarCompensator staticVarCompensator,
                                                     VoltageLevel voltageLevel,
                                                     ReportNode subReportNode) {
        if (staticVarCompensatorCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, staticVarCompensatorCreationInfos.getEquipmentName(), "Name");
        }

        reportStaticVarCompensatorConnectivity(staticVarCompensatorCreationInfos, subReportNode);
        createCompensatorVoltageRegulation(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);
        reportStaticVarCompensatorStandByAutomate(staticVarCompensatorCreationInfos, staticVarCompensator, subReportNode);
    }

    private void reportStaticVarCompensatorConnectivity(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos, ReportNode subReporter) {
        if (staticVarCompensatorCreationInfos.getVoltageLevelId() == null || staticVarCompensatorCreationInfos.getBusOrBusbarSectionId() == null) {
            return;
        }

        if (staticVarCompensatorCreationInfos.getConnectionName() != null ||
                staticVarCompensatorCreationInfos.getConnectionDirection() != null ||
                staticVarCompensatorCreationInfos.getConnectionPosition() != null) {
            List<ReportNode> connectivityReports = new ArrayList<>();
            if (staticVarCompensatorCreationInfos.getConnectionName() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(staticVarCompensatorCreationInfos.getConnectionName(), "Connection name"));
            }
            if (staticVarCompensatorCreationInfos.getConnectionDirection() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(staticVarCompensatorCreationInfos.getConnectionDirection(), "Connection direction"));
            }
            if (staticVarCompensatorCreationInfos.getConnectionPosition() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(staticVarCompensatorCreationInfos.getConnectionPosition(), "Connection position"));
            }
            if (!staticVarCompensatorCreationInfos.isTerminalConnected()) {
                connectivityReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", staticVarCompensatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "ConnectivityCreated", CONNECTIVITY, Map.of());
        }
    }

    private void reportStaticVarCompensatorStandByAutomate(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
                                                           StaticVarCompensator staticVarCompensator, ReportNode subReportNode) {
        if (Boolean.TRUE.equals(staticVarCompensatorCreationInfos.isStandByAutomateOn())) {
            List<ReportNode> standByAutomateReports = new ArrayList<>();
            try {
                staticVarCompensator.newExtension(StandbyAutomatonAdder.class)
                        .withStandbyStatus(staticVarCompensatorCreationInfos.isStandby())
                        .withB0(staticVarCompensatorCreationInfos.getB0())
                        .withLowVoltageSetpoint(staticVarCompensatorCreationInfos.getLowVoltageSetpoint())
                        .withHighVoltageSetpoint(staticVarCompensatorCreationInfos.getHighVoltageSetpoint())
                        .withLowVoltageThreshold(staticVarCompensatorCreationInfos.getLowVoltageThreshold())
                        .withHighVoltageThreshold(staticVarCompensatorCreationInfos.getHighVoltageThreshold())
                        .add();
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.isStandby(),
                        "StandBy"));
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getB0(),
                        "B0"));
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getLowVoltageSetpoint(),
                        "Low voltage setpoint"));
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getHighVoltageSetpoint(),
                        "High voltage setpoint"));
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getHighVoltageThreshold(),
                        "High voltage threshold"));
                standByAutomateReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getLowVoltageThreshold(),
                        "Low voltage threshold"));
            } catch (PowsyblException e) {
                standByAutomateReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("StandbyAutomatonExtensionAddError",
                                "cannot add standby automaton extension on static var compensator with id=${id} : ${message}")
                        .withUntypedValue("id", staticVarCompensatorCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());

            }
            ModificationUtils.getInstance().reportModifications(subReportNode, standByAutomateReports,
                    "StandByAutomateCreated", "stand by automate", Map.of());
        }
    }

    private void createStaticVarCompensatorInBusBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
               ReportNode subReportNode) {

        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, staticVarCompensatorCreationInfos.getBusOrBusbarSectionId());
        /* creating the static var compensator */
        StaticVarCompensator staticVarCompensator = voltageLevel.newStaticVarCompensator()
                .setId(staticVarCompensatorCreationInfos.getEquipmentId())
                .setName(staticVarCompensatorCreationInfos.getEquipmentName())
                .setBus(bus.getId())
                .setConnectableBus(bus.getId())
                .setBmax(staticVarCompensatorCreationInfos.getMaxSusceptance())
                .setBmin(staticVarCompensatorCreationInfos.getMinSusceptance())
                .setVoltageSetpoint(staticVarCompensatorCreationInfos.getVoltageSetpoint())
                .setReactivePowerSetpoint(staticVarCompensatorCreationInfos.getReactivePowerSetpoint())
                .setRegulationMode(staticVarCompensatorCreationInfos.getRegulationMode())
                .add();

        addExtensionsToStaticVarCompensator(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("staticVarCompensatorCreated", "New static var compensator with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void createCompensatorVoltageRegulation(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
                                                    StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        voltageReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("VoltageRegulationOn", modificationInfos.getVoltageRegulationType() == VoltageRegulationType.DISTANT &&
                        modificationInfos.getRegulationMode() == StaticVarCompensator.RegulationMode.VOLTAGE));
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getVoltageSetpoint(), "Voltage setpoint"));
        if (staticVarCompensatorCreationInfos.getRegulatingTerminalVlId() != null && staticVarCompensatorCreationInfos.getRegulatingTerminalId() != null &&
                staticVarCompensatorCreationInfos.getRegulatingTerminalType() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                    staticVarCompensatorCreationInfos.getRegulatingTerminalId(),
                    staticVarCompensatorCreationInfos.getRegulatingTerminalType(),
                    staticVarCompensatorCreationInfos.getRegulatingTerminalVlId());
            if (terminal != null) {
                updateCompensatorRegulatingTerminal(staticVarCompensatorCreationInfos, staticVarCompensator, terminal, voltageReports);
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "VoltageRegulationCreated", "Voltage regulation", Map.of());
    }

    private void updateCompensatorRegulatingTerminal(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos, StaticVarCompensator staticVarCompensator,
                                                   Terminal terminal, List<ReportNode> voltageReports) {
        if (staticVarCompensatorCreationInfos.getRegulatingTerminalId() != null
                && staticVarCompensatorCreationInfos.getRegulatingTerminalType() != null
                && staticVarCompensatorCreationInfos.getRegulatingTerminalVlId() != null) {
            staticVarCompensator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationInfos.getRegulatingTerminalVlId(),
                    "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    staticVarCompensatorCreationInfos.getRegulatingTerminalType() + ":"
                            + staticVarCompensatorCreationInfos.getRegulatingTerminalId(),
                    "Equipment"));
        }
    }

}
