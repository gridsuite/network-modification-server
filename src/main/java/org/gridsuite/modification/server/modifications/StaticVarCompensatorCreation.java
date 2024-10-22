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
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.StandbyAutomatonAdder;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.StaticVarCompensatorCreationInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.gridsuite.modification.server.NetworkModificationException.Type.STATIC_VAR_COMPENSATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.*;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
public class StaticVarCompensatorCreation extends AbstractModification {

    private final StaticVarCompensatorCreationInfos modificationInfos;

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
        ModificationUtils.getInstance().checkReactivePowerLimitsAndSetPointsCreation(modificationInfos);

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(), modificationInfos.getRegulatingTerminalId(),
                modificationInfos.getRegulatingTerminalType(), modificationInfos.getRegulatingTerminalVlId());

        // check standby automaton
        ModificationUtils.getInstance().checkStandbyAutomatonCreation(modificationInfos);
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the static var compensator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createStaticVarCompensatorInNodeBreaker(voltageLevel, modificationInfos, network, subReportNode);
        } else {
            createStaticVarCompensatorInBusBreaker(voltageLevel, modificationInfos, subReportNode);
        }
        ModificationUtils.getInstance().disconnectCreatedInjection(modificationInfos, network.getStaticVarCompensator(modificationInfos.getEquipmentId()), subReportNode);
        // properties
        StaticVarCompensator staticVarCompensator = network.getStaticVarCompensator(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(staticVarCompensator, subReportNode, modificationInfos.getProperties(), "StaticVarCompensatorProperties");
        subReportNode.newReportNode()
                .withMessageTemplate("staticVarCompensatorCreated", "New static var compensator with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private void createStaticVarCompensatorInNodeBreaker(VoltageLevel voltageLevel, StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
            Network network, ReportNode subReportNode) {
        StaticVarCompensatorAdder staticVarCompensatorAdder = createStaticVarCompensatorAdderInNodeBreaker(voltageLevel, staticVarCompensatorCreationInfos);
        createInjectionInNodeBreaker(voltageLevel, staticVarCompensatorCreationInfos, network, staticVarCompensatorAdder, subReportNode);
        var staticVarCompensator = ModificationUtils.getInstance().getStaticVarCompensator(network, staticVarCompensatorCreationInfos.getEquipmentId());
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
                .setBmax(Objects.isNull(staticVarCompensatorCreationInfos.getMaxSusceptance()) && Objects.nonNull(staticVarCompensatorCreationInfos.getMaxQAtNominalV()) ?
                        (staticVarCompensatorCreationInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : nanIfNull(staticVarCompensatorCreationInfos.getMaxSusceptance()))
                .setBmin(Objects.isNull(staticVarCompensatorCreationInfos.getMinSusceptance()) && Objects.nonNull(staticVarCompensatorCreationInfos.getMinQAtNominalV()) ?
                        (staticVarCompensatorCreationInfos.getMinQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : nanIfNull(staticVarCompensatorCreationInfos.getMinSusceptance()))
                .setVoltageSetpoint(nanIfNull(staticVarCompensatorCreationInfos.getVoltageSetpoint()))
                .setReactivePowerSetpoint(nanIfNull(staticVarCompensatorCreationInfos.getReactivePowerSetpoint()))
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

        reportInjectionCreationConnectivity(staticVarCompensatorCreationInfos, subReportNode);
        reportStaticVarCompensatorLimitsAndSetpoints(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);
        reportStaticVarCompensatorStandbyAutomaton(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorStandbyAutomaton(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
                                                            StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (Boolean.TRUE.equals(staticVarCompensatorCreationInfos.isStandbyAutomatonOn())) {
            List<ReportNode> standbyAutomatonReports = new ArrayList<>();
            try {
                staticVarCompensator.newExtension(StandbyAutomatonAdder.class)
                        .withStandbyStatus(staticVarCompensatorCreationInfos.isStandby())
                        .withB0(Objects.isNull(staticVarCompensatorCreationInfos.getB0()) && Objects.nonNull(staticVarCompensatorCreationInfos.getQ0()) ?
                                (staticVarCompensatorCreationInfos.getQ0()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationInfos.getB0())
                        .withLowVoltageSetpoint(staticVarCompensatorCreationInfos.getLowVoltageSetpoint())
                        .withHighVoltageSetpoint(staticVarCompensatorCreationInfos.getHighVoltageSetpoint())
                        .withLowVoltageThreshold(staticVarCompensatorCreationInfos.getLowVoltageThreshold())
                        .withHighVoltageThreshold(staticVarCompensatorCreationInfos.getHighVoltageThreshold())
                        .add();
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.isStandby(),
                        "Standby"));
                if (Objects.nonNull(staticVarCompensatorCreationInfos.getB0())) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                            staticVarCompensatorCreationInfos.getB0(),
                            "Fixed part of susceptance"));
                }
                if (Objects.nonNull(staticVarCompensatorCreationInfos.getQ0())) {
                    standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                            staticVarCompensatorCreationInfos.getQ0(),
                            "Fixed part of Q at nominal voltage"));
                }
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getLowVoltageSetpoint(),
                        "Low voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getHighVoltageSetpoint(),
                        "High voltage set point"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getHighVoltageThreshold(),
                        "High voltage threshold"));
                standbyAutomatonReports.add(ModificationUtils.getInstance().buildCreationReport(
                        staticVarCompensatorCreationInfos.getLowVoltageThreshold(),
                        "Low voltage threshold"));
            } catch (PowsyblException e) {
                standbyAutomatonReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("StandbyAutomatonExtensionAddError",
                                "Cannot add standby automaton extension on static var compensator with id=${id} : ${message}")
                        .withUntypedValue("id", staticVarCompensatorCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());

            }
            ModificationUtils.getInstance().reportModifications(subReportNode, standbyAutomatonReports,
                    "StandbyAutomatonCreated", "Standby automaton");
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
                .setBmax(Objects.isNull(staticVarCompensatorCreationInfos.getMaxSusceptance()) && Objects.nonNull(staticVarCompensatorCreationInfos.getMaxQAtNominalV()) ?
                        (staticVarCompensatorCreationInfos.getMaxQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationInfos.getMaxSusceptance())
                .setBmin(Objects.isNull(staticVarCompensatorCreationInfos.getMinSusceptance()) && Objects.nonNull(staticVarCompensatorCreationInfos.getMinQAtNominalV()) ?
                        (staticVarCompensatorCreationInfos.getMinQAtNominalV()) / Math.pow(voltageLevel.getNominalV(), 2) : staticVarCompensatorCreationInfos.getMinSusceptance())
                .setVoltageSetpoint(staticVarCompensatorCreationInfos.getVoltageSetpoint())
                .setReactivePowerSetpoint(staticVarCompensatorCreationInfos.getReactivePowerSetpoint())
                .setRegulationMode(staticVarCompensatorCreationInfos.getRegulationMode())
                .add();

        addExtensionsToStaticVarCompensator(staticVarCompensatorCreationInfos, staticVarCompensator, voltageLevel, subReportNode);
    }

    private void reportStaticVarCompensatorLimitsAndSetpoints(StaticVarCompensatorCreationInfos staticVarCompensatorCreationInfos,
                                                              StaticVarCompensator staticVarCompensator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getMinSusceptance())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getMinSusceptance(), "Susceptance min"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getMaxSusceptance())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getMaxSusceptance(), "Susceptance max"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getMinQAtNominalV())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getMinQAtNominalV(), "Q min at nominal voltage"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getMaxQAtNominalV())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getMaxQAtNominalV(), "Q max at nominal voltage"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getRegulationMode())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getRegulationMode(), "regulation mode"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getVoltageSetpoint())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getVoltageSetpoint(), "Voltage set point"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getReactivePowerSetpoint())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getReactivePowerSetpoint(), "Reactive power set point"));
        }
        if (Objects.nonNull(staticVarCompensatorCreationInfos.getVoltageRegulationType())) {
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(staticVarCompensatorCreationInfos.getVoltageRegulationType(), "Voltage Regulation type"));
        }
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
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "LimitsAndSetpointsCreated", "Limits and Setpoints");
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
