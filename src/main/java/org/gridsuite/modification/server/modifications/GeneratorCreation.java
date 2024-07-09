/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
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
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.GeneratorAdder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import com.powsybl.network.store.iidm.impl.extensions.GeneratorStartupAdderImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATOR_ALREADY_EXISTS;
import static org.gridsuite.modification.server.modifications.ModificationUtils.nanIfNull;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorCreation extends AbstractModification {

    private final GeneratorCreationInfos modificationInfos;
    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String CONNECTIVITY = "Connectivity";

    public GeneratorCreation(GeneratorCreationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getGenerator(modificationInfos.getEquipmentId()) != null) {
            throw new NetworkModificationException(GENERATOR_ALREADY_EXISTS, modificationInfos.getEquipmentId());
        }

        // check connectivity
        ModificationUtils.getInstance().controlConnectivity(network, modificationInfos.getVoltageLevelId(),
                modificationInfos.getBusOrBusbarSectionId(), modificationInfos.getConnectionPosition());

        // check reactive limits
        ModificationUtils.getInstance().checkReactiveLimitsCreation(modificationInfos,
                modificationInfos.getErrorType(),
                modificationInfos.getEquipmentId(),
                "Generator");

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            modificationInfos.getRegulatingTerminalId(),
            modificationInfos.getRegulatingTerminalType(),
            modificationInfos.getRegulatingTerminalVlId());
    }

    @Override
    public void apply(Network network, ReportNode subReportNode) {
        // create the generator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createGeneratorInNodeBreaker(voltageLevel, modificationInfos, network, subReportNode);
        } else {
            createGeneratorInBusBreaker(voltageLevel, modificationInfos, subReportNode);
        }
        if (!modificationInfos.isTerminalConnected()) {
            network.getGenerator(modificationInfos.getEquipmentId()).getTerminal().disconnect();
        }
        // apply the properties
        Generator generator = network.getGenerator(modificationInfos.getEquipmentId());
        PropertiesUtils.applyProperties(generator, subReportNode, modificationInfos.getProperties());
    }

    private void createGeneratorInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos, Network network, ReportNode subReportNode) {
        GeneratorAdder generatorAdder = createGeneratorAdderInNodeBreaker(voltageLevel, generatorCreationInfos);
        var position = ModificationUtils.getInstance().getPosition(generatorCreationInfos.getConnectionPosition(),
                generatorCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);

        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBusOrBusbarSectionId(generatorCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(generatorCreationInfos.getConnectionDirection())
                .withInjectionFeederName(generatorCreationInfos.getConnectionName() != null
                        ? generatorCreationInfos.getConnectionName()
                        : generatorCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(generatorAdder)
                .build();

        algo.apply(network, true, subReportNode);

        // CreateFeederBayBuilder already create the generator using
        // (withInjectionAdder(generatorAdder)) so then we can add the additional informations and extensions
        var generator = ModificationUtils.getInstance().getGenerator(network, generatorCreationInfos.getEquipmentId());
        addExtensionsToGenerator(generatorCreationInfos, generator, voltageLevel, subReportNode);
    }

    private GeneratorAdder createGeneratorAdderInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos) {
        Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            generatorCreationInfos.getRegulatingTerminalId(),
            generatorCreationInfos.getRegulatingTerminalType(),
            generatorCreationInfos.getRegulatingTerminalVlId());

        // creating the generator adder
        GeneratorAdder generatorAdder = voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setMinP(generatorCreationInfos.getMinP())
            .setMaxP(generatorCreationInfos.getMaxP())
            .setRatedS(nanIfNull(generatorCreationInfos.getRatedS()))
            .setTargetP(generatorCreationInfos.getTargetP())
            .setTargetQ(nanIfNull(generatorCreationInfos.getTargetQ()))
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationInfos.getTargetV()));

        if (terminal != null) {
            generatorAdder.setRegulatingTerminal(terminal);
        }

        return generatorAdder;
    }

    private void addExtensionsToGenerator(GeneratorCreationInfos generatorCreationInfos, Generator generator,
                                          VoltageLevel voltageLevel, ReportNode subReportNode) {
        if (generatorCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, generatorCreationInfos.getEquipmentName(), "Name");
        }
        if (generatorCreationInfos.getEnergySource() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReportNode, generatorCreationInfos.getEnergySource(), "Energy source");
        }
        reportGeneratorConnectivity(generatorCreationInfos, subReportNode);
        ReportNode subReporterLimits = reportGeneratorActiveLimits(generatorCreationInfos, subReportNode);
        ModificationUtils.getInstance().createReactiveLimits(generatorCreationInfos, generator, subReporterLimits);
        ReportNode subReporterSetpoints = reportGeneratorSetPoints(generatorCreationInfos, subReportNode);
        createGeneratorVoltageRegulation(generatorCreationInfos, generator, voltageLevel, subReporterSetpoints);
        createGeneratorActivePowerControl(generatorCreationInfos, generator, subReporterSetpoints);
        createGeneratorShortCircuit(generatorCreationInfos, generator, subReportNode);
        createGeneratorStartUp(generatorCreationInfos, generator, subReportNode);
    }

    private void createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos, ReportNode subReportNode) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId());

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setMinP(generatorCreationInfos.getMinP())
            .setMaxP(generatorCreationInfos.getMaxP())
            .setRatedS(nanIfNull(generatorCreationInfos.getRatedS()))
            .setTargetP(generatorCreationInfos.getTargetP())
            .setTargetQ(nanIfNull(generatorCreationInfos.getTargetQ()))
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationInfos.getTargetV()))
            .add();

        addExtensionsToGenerator(generatorCreationInfos, generator, voltageLevel, subReportNode);

        subReportNode.newReportNode()
                .withMessageTemplate("generatorCreated", "New generator with id=${id} created")
                .withUntypedValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();
    }

    private ReportNode reportGeneratorSetPoints(GeneratorCreationInfos generatorCreationInfos, ReportNode subReportNode) {
        List<ReportNode> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(generatorCreationInfos.getTargetP(), "Active power"));
        if (generatorCreationInfos.getTargetQ() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(generatorCreationInfos.getTargetQ(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReportNode, setPointReports, "SetPointCreated", "Setpoints", Map.of());
    }

    private void createGeneratorVoltageRegulation(GeneratorCreationInfos generatorCreationInfos, Generator generator, VoltageLevel voltageLevel, ReportNode subReportNode) {
        List<ReportNode> voltageReports = new ArrayList<>();
        voltageReports.add(ModificationUtils.getInstance()
                .createEnabledDisabledReport("VoltageRegulationOn", modificationInfos.isVoltageRegulationOn()));
        voltageReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getTargetV(), "Voltage"));
        if (generatorCreationInfos.getRegulatingTerminalVlId() != null && generatorCreationInfos.getRegulatingTerminalId() != null &&
                generatorCreationInfos.getRegulatingTerminalType() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                    generatorCreationInfos.getRegulatingTerminalId(),
                    generatorCreationInfos.getRegulatingTerminalType(),
                    generatorCreationInfos.getRegulatingTerminalVlId());
            if (terminal != null) {
                updateGeneratorRegulatingTerminal(generatorCreationInfos, generator, terminal, voltageReports);
            }
        }
        if (generatorCreationInfos.getQPercent() != null) {
            try {
                generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                        .withQPercent(generatorCreationInfos.getQPercent()).add();
                voltageReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getQPercent(), "Reactive percentage"));
            } catch (PowsyblException e) {
                voltageReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("ReactivePercentageError", "cannot add Coordinated reactive extension on generator with id=${id} : ${message}")
                        .withUntypedValue("id", generatorCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
        }
        ModificationUtils.getInstance().reportModifications(subReportNode, voltageReports, "VoltageRegulationCreated", "Voltage regulation", Map.of());

    }

    private void updateGeneratorRegulatingTerminal(GeneratorCreationInfos generatorCreationInfos, Generator generator,
                                                   Terminal terminal, List<ReportNode> voltageReports) {
        if (generatorCreationInfos.getRegulatingTerminalId() != null
                && generatorCreationInfos.getRegulatingTerminalType() != null
                && generatorCreationInfos.getRegulatingTerminalVlId() != null) {
            generator.setRegulatingTerminal(terminal);
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    generatorCreationInfos.getRegulatingTerminalVlId(),
                    "Voltage level"));
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(
                    generatorCreationInfos.getRegulatingTerminalType() + ":"
                            + generatorCreationInfos.getRegulatingTerminalId(),
                    "Equipment"));
        }
    }

    private void reportGeneratorConnectivity(GeneratorCreationInfos generatorCreationInfos, ReportNode subReporter) {
        if (generatorCreationInfos.getVoltageLevelId() == null || generatorCreationInfos.getBusOrBusbarSectionId() == null) {
            return;
        }

        if (generatorCreationInfos.getConnectionName() != null ||
            generatorCreationInfos.getConnectionDirection() != null ||
            generatorCreationInfos.getConnectionPosition() != null) {
            List<ReportNode> connectivityReports = new ArrayList<>();
            if (generatorCreationInfos.getConnectionName() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionName(), "Connection name"));
            }
            if (generatorCreationInfos.getConnectionDirection() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionDirection(), "Connection direction"));
            }
            if (generatorCreationInfos.getConnectionPosition() != null) {
                connectivityReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionPosition(), "Connection position"));
            }
            if (!generatorCreationInfos.isTerminalConnected()) {
                connectivityReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("equipmentDisconnected", "    Equipment with id=${id} disconnected")
                        .withUntypedValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.INFO_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, connectivityReports, "ConnectivityCreated", CONNECTIVITY, Map.of());
        }
    }

    private ReportNode reportGeneratorActiveLimits(GeneratorCreationInfos generatorCreationInfos, ReportNode subReportNode) {
        List<ReportNode> limitsReports = new ArrayList<>();
        ReportNode subReportNodeLimits = subReportNode.newReportNode().withMessageTemplate(LIMITS, LIMITS).add();
        subReportNodeLimits.newReportNode()
                .withMessageTemplate(LIMITS, LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .add();

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationInfos.getMinP(), "Min active power"));

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationInfos.getMaxP(), "Max active power"));

        if (generatorCreationInfos.getRatedS() != null) {
            limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationInfos.getRatedS(), "Rated nominal power"));
        }
        ModificationUtils.getInstance().reportModifications(subReportNodeLimits, limitsReports, "ActiveLimitsCreated", ACTIVE_LIMITS, Map.of());
        return subReportNodeLimits;
    }

    private void createGeneratorActivePowerControl(GeneratorCreationInfos generatorCreationInfos, Generator generator, ReportNode subReportNode) {
        if (generatorCreationInfos.getParticipate() != null && generatorCreationInfos.getDroop() != null) {
            List<ReportNode> activePowerRegulationReports = new ArrayList<>();
            try {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(generatorCreationInfos.getParticipate())
                        .withDroop(generatorCreationInfos.getDroop())
                        .add();
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getParticipate(),
                        "Participate"));
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getDroop(),
                        "Droop"));
            } catch (PowsyblException e) {
                activePowerRegulationReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("ActivePowerExtensionAddError", "cannot add active power extension on generator with id=${id} : ${message}")
                        .withUntypedValue("id", generatorCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());

            }
            ModificationUtils.getInstance().reportModifications(subReportNode, activePowerRegulationReports, "ActivePowerRegulationCreated", "Active power regulation", Map.of());
        }
    }

    private void createGeneratorShortCircuit(GeneratorCreationInfos generatorCreationInfos, Generator generator, ReportNode subReportNode) {
        if (generatorCreationInfos.getDirectTransX() != null) {
            List<ReportNode> shortCircuitReports = new ArrayList<>();
            try {
                GeneratorShortCircuitAdder shortCircuitAdder = generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorCreationInfos.getDirectTransX());
                if (generatorCreationInfos.getStepUpTransformerX() != null) {
                    shortCircuitAdder.withStepUpTransformerX(generatorCreationInfos.getStepUpTransformerX());
                }
                shortCircuitAdder.add();
                shortCircuitReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getDirectTransX(), "Transient reactance"));
                if (generatorCreationInfos.getStepUpTransformerX() != null) {
                    shortCircuitReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getStepUpTransformerX(), "Transformer reactance"));
                }
            } catch (PowsyblException e) {
                shortCircuitReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("ShortCircuitExtensionAddError", "cannot add short-circuit extension on generator with id=${id} : ${message}")
                        .withUntypedValue("id", generatorCreationInfos.getEquipmentId())
                        .withUntypedValue("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, shortCircuitReports, "shortCircuitCreated", "Short-circuit", Map.of());
        }
    }

    private void createGeneratorStartUp(GeneratorCreationInfos generatorCreationInfos, Generator generator, ReportNode subReportNode) {
        if (generatorCreationInfos.getPlannedActivePowerSetPoint() != null
                || generatorCreationInfos.getMarginalCost() != null
                || generatorCreationInfos.getPlannedOutageRate() != null
                || generatorCreationInfos.getForcedOutageRate() != null) {
            List<ReportNode> startupReports = new ArrayList<>();
            try {
                generator.newExtension(GeneratorStartupAdderImpl.class)
                        .withPlannedActivePowerSetpoint(nanIfNull(generatorCreationInfos.getPlannedActivePowerSetPoint()))
                        .withMarginalCost(nanIfNull(generatorCreationInfos.getMarginalCost()))
                        .withPlannedOutageRate(nanIfNull(generatorCreationInfos.getPlannedOutageRate()))
                        .withForcedOutageRate(nanIfNull(generatorCreationInfos.getForcedOutageRate()))
                        .add();
                if (generatorCreationInfos.getPlannedActivePowerSetPoint() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getPlannedActivePowerSetPoint(), "Planning active power set point"));
                }
                if (generatorCreationInfos.getMarginalCost() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getMarginalCost(), "Marginal cost"));
                }
                if (generatorCreationInfos.getPlannedOutageRate() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getPlannedOutageRate(), "Planning outage rate"));
                }
                if (generatorCreationInfos.getForcedOutageRate() != null) {
                    startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getForcedOutageRate(), "Forced outage rate"));
                }
            } catch (PowsyblException e) {
                startupReports.add(ReportNode.newRootReportNode()
                        .withMessageTemplate("StartupExtensionAddError", "cannot add startup extension on generator with id=${id} : ${message}")
                        .withUntypedValue("id", generatorCreationInfos.getEquipmentId())
                        .withMessageTemplate("message", e.getMessage())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReportNode, startupReports, "startUpAttributesCreated", "Start up", Map.of());
        }
    }
}
