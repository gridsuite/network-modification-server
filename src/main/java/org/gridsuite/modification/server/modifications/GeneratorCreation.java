/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.commons.PowsyblException;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.server.NetworkModificationException;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.modifications.ModificationUtils.nanIfNull;

import org.gridsuite.modification.server.dto.GeneratorCreationInfos;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.modification.topology.CreateFeederBay;
import com.powsybl.iidm.modification.topology.CreateFeederBayBuilder;
import com.powsybl.iidm.network.Bus;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.GeneratorAdder;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurveAdder;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import com.powsybl.network.store.iidm.impl.extensions.GeneratorStartupAdderImpl;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorCreation extends AbstractModification {

    private final GeneratorCreationInfos modificationInfos;
    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String REACTIVE_LIMITS = "Reactive limits";
    private static final String CONNECTION = "Connection";

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

        // check min max reactive limits
        if (modificationInfos.getMinimumReactivePower() != null && modificationInfos.getMaximumReactivePower() != null) {
            if (Double.isNaN(modificationInfos.getMinimumReactivePower())) {
                throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : minimum reactive power is not set");
            } else if (Double.isNaN(modificationInfos.getMaximumReactivePower())) {
                throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : maximum reactive power is not set");
            } else if (modificationInfos.getMaximumReactivePower() < modificationInfos.getMinimumReactivePower()) {
                throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : maximum reactive power is expected to be greater than or equal to minimum reactive power");
            }
        }

        // check reactive capability curve limits
        List<ReactiveCapabilityCurveCreationInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        if (!CollectionUtils.isEmpty(points)) {
            if (points.size() < 2) {
                throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : a reactive capability curve should have at least two points");
            }
            IntStream.range(0, points.size())
                .forEach(i -> {
                    ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                    if (Double.isNaN(newPoint.getP())) {
                        throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : P is not set in a reactive capability curve limits point");
                    } else if (Double.isNaN(newPoint.getQminP())) {
                        throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : min Q is not set in a reactive capability curve limits point");
                    } else if (Double.isNaN(newPoint.getQmaxP())) {
                        throw new NetworkModificationException(CREATE_GENERATOR_ERROR, "Generator '" + modificationInfos.getEquipmentId() + "' : max Q is not set in a reactive capability curve limits point");
                    }
                });
        }

        // check regulated terminal
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
            modificationInfos.getRegulatingTerminalId(),
            modificationInfos.getRegulatingTerminalType(),
            modificationInfos.getRegulatingTerminalVlId());
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        // create the generator in the network
        VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getVoltageLevelId());
        if (voltageLevel.getTopologyKind() == TopologyKind.NODE_BREAKER) {
            createGeneratorInNodeBreaker(voltageLevel, modificationInfos, network, subReporter);
        } else {
            createGeneratorInBusBreaker(voltageLevel, modificationInfos, subReporter);
        }
    }

    private void createGeneratorInNodeBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos, Network network, Reporter subReporter) {
        GeneratorAdder generatorAdder = createGeneratorAdderInNodeBreaker(voltageLevel, generatorCreationInfos);
        var position = ModificationUtils.getInstance().getPosition(generatorCreationInfos.getConnectionPosition(),
                generatorCreationInfos.getBusOrBusbarSectionId(), network, voltageLevel);

        CreateFeederBay algo = new CreateFeederBayBuilder()
                .withBbsId(generatorCreationInfos.getBusOrBusbarSectionId())
                .withInjectionDirection(generatorCreationInfos.getConnectionDirection())
                .withInjectionFeederName(generatorCreationInfos.getConnectionName() != null
                        ? generatorCreationInfos.getConnectionName()
                        : generatorCreationInfos.getEquipmentId())
                .withInjectionPositionOrder(position)
                .withInjectionAdder(generatorAdder)
                .build();

        algo.apply(network, true, subReporter);

        // CreateFeederBayBuilder already create the generator using
        // (withInjectionAdder(generatorAdder)) so then we can add the additional informations and extensions
        var generator = ModificationUtils.getInstance().getGenerator(network, generatorCreationInfos.getEquipmentId());
        addExtensionsToGenerator(generatorCreationInfos, generator, voltageLevel, subReporter);
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
            .setMinP(generatorCreationInfos.getMinActivePower())
            .setMaxP(generatorCreationInfos.getMaxActivePower())
            .setRatedS(nanIfNull(generatorCreationInfos.getRatedNominalPower()))
            .setTargetP(generatorCreationInfos.getActivePowerSetpoint())
            .setTargetQ(nanIfNull(generatorCreationInfos.getReactivePowerSetpoint()))
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationInfos.getVoltageSetpoint()));

        if (terminal != null) {
            generatorAdder.setRegulatingTerminal(terminal);
        }

        return generatorAdder;
    }

    private void addExtensionsToGenerator(GeneratorCreationInfos generatorCreationInfos, Generator generator, VoltageLevel voltageLevel, Reporter subReporter) {
        if (generatorCreationInfos.getEquipmentName() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReporter, generatorCreationInfos.getEquipmentName(), "Name");
        }
        if (generatorCreationInfos.getEnergySource() != null) {
            ModificationUtils.getInstance().reportElementaryCreation(subReporter, generatorCreationInfos.getEnergySource(), "Energy source");
        }
        reportGeneratorConnection(generatorCreationInfos, subReporter);
        Reporter subReporterLimits = reportGeneratorActiveLimits(generatorCreationInfos, subReporter);
        createGeneratorReactiveLimits(generatorCreationInfos, generator, subReporterLimits);
        Reporter subReporterSetpoints = reportGeneratorSetPoints(generatorCreationInfos, subReporter);
        createGeneratorVoltageRegulation(generatorCreationInfos, generator, voltageLevel, subReporterSetpoints);
        createGeneratorActivePowerControl(generatorCreationInfos, generator, subReporterSetpoints);
        createGeneratorShortCircuit(generatorCreationInfos, generator, subReporter);
        createGeneratorStartUp(generatorCreationInfos, generator, subReporter);
    }

    private void createGeneratorInBusBreaker(VoltageLevel voltageLevel, GeneratorCreationInfos generatorCreationInfos, Reporter subReporter) {
        Bus bus = ModificationUtils.getInstance().getBusBreakerBus(voltageLevel, generatorCreationInfos.getBusOrBusbarSectionId());

        // creating the generator
        Generator generator = voltageLevel.newGenerator()
            .setId(generatorCreationInfos.getEquipmentId())
            .setName(generatorCreationInfos.getEquipmentName())
            .setEnergySource(generatorCreationInfos.getEnergySource())
            .setBus(bus.getId())
            .setConnectableBus(bus.getId())
            .setMinP(generatorCreationInfos.getMinActivePower())
            .setMaxP(generatorCreationInfos.getMaxActivePower())
            .setRatedS(nanIfNull(generatorCreationInfos.getRatedNominalPower()))
            .setTargetP(generatorCreationInfos.getActivePowerSetpoint())
            .setTargetQ(nanIfNull(generatorCreationInfos.getReactivePowerSetpoint()))
            .setVoltageRegulatorOn(generatorCreationInfos.isVoltageRegulationOn())
            .setTargetV(nanIfNull(generatorCreationInfos.getVoltageSetpoint()))
            .add();

        addExtensionsToGenerator(generatorCreationInfos, generator, voltageLevel, subReporter);

        subReporter.report(Report.builder()
                .withKey("generatorCreated")
                .withDefaultMessage("New generator with id=${id} created")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
    }

    private void addToReports(List<Report> reports, Double newValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildCreationReport(newValue, fieldName));
        }
    }

    private void createGeneratorReactiveLimits(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        if (Boolean.TRUE.equals(generatorCreationInfos.getReactiveCapabilityCurve())) {
            createReactiveCapabilityCurve(generatorCreationInfos, generator, subReporter);
        } else if (Boolean.FALSE.equals(generatorCreationInfos.getReactiveCapabilityCurve())) {
            createMinMaxReactiveLimits(generatorCreationInfos, generator, subReporter);
        }
    }

    private void createMinMaxReactiveLimits(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        List<Report> minMaxReactiveLimitsReports = new ArrayList<>();
        if (generatorCreationInfos.getMinimumReactivePower() != null && generatorCreationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(generatorCreationInfos.getMinimumReactivePower())
                .setMaxQ(generatorCreationInfos.getMaximumReactivePower())
                .add();
            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationInfos.getMinimumReactivePower(),
                "Minimum reactive power"));
            minMaxReactiveLimitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationInfos.getMaximumReactivePower(),
                "Maximum reactive power"));
            Reporter subReporterReactiveLimits = subReporter.createSubReporter(REACTIVE_LIMITS, REACTIVE_LIMITS);
            subReporterReactiveLimits.report(Report.builder()
                .withKey(REACTIVE_LIMITS)
                .withDefaultMessage(REACTIVE_LIMITS)
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());
            ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, minMaxReactiveLimitsReports, "minMaxReactiveLimitsCreated", "By range");
        }
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder, ReactiveCapabilityCurveCreationInfos point,
                                                    List<Report> reports, String fieldSuffix) {
        adder.beginPoint()
            .setMaxQ(point.getQmaxP())
            .setMinQ(point.getQminP())
            .setP(point.getP())
            .endPoint();
        addToReports(reports, point.getP(), "P" + fieldSuffix);
        addToReports(reports, point.getQminP(), "QminP" + fieldSuffix);
        addToReports(reports, point.getQmaxP(), "QmaxP" + fieldSuffix);
    }

    private void createReactiveCapabilityCurve(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        List<Report> pointsReports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveCreationInfos> points = generatorCreationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
            .forEach(i -> {
                String fieldSuffix;
                ReactiveCapabilityCurveCreationInfos newPoint = points.get(i);
                if (i == 0) {
                    fieldSuffix = "min";
                } else if (i == (points.size() - 1)) {
                    fieldSuffix = "max";
                } else {
                    fieldSuffix = Integer.toString(i);
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

    private Reporter reportGeneratorSetPoints(GeneratorCreationInfos generatorCreationInfos, Reporter subReporter) {
        List<Report> setPointReports = new ArrayList<>();
        setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(generatorCreationInfos.getActivePowerSetpoint(), "Active power"));
        if (generatorCreationInfos.getReactivePowerSetpoint() != null) {
            setPointReports.add(ModificationUtils.getInstance()
                .buildCreationReport(generatorCreationInfos.getReactivePowerSetpoint(), "Reactive power"));
        }
        return ModificationUtils.getInstance().reportModifications(subReporter, setPointReports, "SetPointCreated", "Setpoints");
    }

    private void createGeneratorVoltageRegulation(GeneratorCreationInfos generatorCreationInfos, Generator generator, VoltageLevel voltageLevel, Reporter subReporter) {
        if (generatorCreationInfos.isVoltageRegulationOn()) {
            List<Report> voltageReports = new ArrayList<>();
            voltageReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getVoltageSetpoint(), "Voltage"));
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
                    voltageReports.add(Report.builder()
                        .withKey("ReactivePercentageError")
                        .withDefaultMessage("cannot add Coordinated reactive extension on generator with id=${id} :" + e.getMessage())
                        .withValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
                }
            }
            ModificationUtils.getInstance().reportModifications(subReporter, voltageReports, "VoltageRegulationCreated", "Voltage regulation");
        }
    }

    private void updateGeneratorRegulatingTerminal(GeneratorCreationInfos generatorCreationInfos, Generator generator,
                                                   Terminal terminal, List<Report> voltageReports) {
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

    private void reportGeneratorConnection(GeneratorCreationInfos generatorCreationInfos, Reporter subReporter) {
        if ((generatorCreationInfos.getVoltageLevelId() != null && generatorCreationInfos.getBusOrBusbarSectionId() != null)
            && (generatorCreationInfos.getConnectionName() != null || generatorCreationInfos.getConnectionDirection() != null ||
                generatorCreationInfos.getConnectionPosition() != null)) {
            List<Report> connectionReports = new ArrayList<>();
            if (generatorCreationInfos.getConnectionName() != null) {
                connectionReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionName(), "Name"));
            }
            if (generatorCreationInfos.getConnectionDirection() != null) {
                connectionReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionDirection(), "Direction"));
            }
            if (generatorCreationInfos.getConnectionPosition() != null) {
                connectionReports.add(ModificationUtils.getInstance()
                        .buildCreationReport(generatorCreationInfos.getConnectionPosition(), "Position"));
            }
            ModificationUtils.getInstance().reportModifications(subReporter, connectionReports, "ConnectionCreated", CONNECTION);
        }
    }

    private Reporter reportGeneratorActiveLimits(GeneratorCreationInfos generatorCreationInfos, Reporter subReporter) {
        List<Report> limitsReports = new ArrayList<>();
        Reporter subReporterLimits = subReporter.createSubReporter(LIMITS, LIMITS);
        subReporterLimits.report(Report.builder()
            .withKey(LIMITS)
            .withDefaultMessage(LIMITS)
            .withSeverity(TypedValue.INFO_SEVERITY)
            .build());
        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationInfos.getMinActivePower(), "Min active power"));

        limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
            generatorCreationInfos.getMaxActivePower(), "Max active power"));

        if (generatorCreationInfos.getRatedNominalPower() != null) {
            limitsReports.add(ModificationUtils.getInstance().buildCreationReport(
                generatorCreationInfos.getRatedNominalPower(), "Rated nominal power"));
        }
        ModificationUtils.getInstance().reportModifications(subReporterLimits, limitsReports, "ActiveLimitsCreated", ACTIVE_LIMITS);
        return subReporterLimits;
    }

    private void createGeneratorActivePowerControl(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        if (generatorCreationInfos.getParticipate() != null && generatorCreationInfos.getDroop() != null) {
            List<Report> activePowerRegulationReports = new ArrayList<>();
            try {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(generatorCreationInfos.getParticipate())
                        .withDroop(generatorCreationInfos.getDroop())
                        .add();
                activePowerRegulationReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getDroop(),
                        "Droop"));
            } catch (PowsyblException e) {
                activePowerRegulationReports.add(Report.builder()
                        .withKey("ActivePowerExtensionAddError")
                        .withDefaultMessage("cannot add active power extension on generator with id=${id} : " + e.getMessage())
                        .withValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, activePowerRegulationReports, "ActivePowerRegulationCreated", "Active power regulation");
        }
    }

    private void createGeneratorShortCircuit(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        if (generatorCreationInfos.getTransientReactance() != null) {
            List<Report> shortCircuitReports = new ArrayList<>();
            try {
                GeneratorShortCircuitAdder shortCircuitAdder = generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorCreationInfos.getTransientReactance());
                if (generatorCreationInfos.getStepUpTransformerReactance() != null) {
                    shortCircuitAdder.withStepUpTransformerX(generatorCreationInfos.getStepUpTransformerReactance());
                }
                shortCircuitAdder.add();
                shortCircuitReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getTransientReactance(), "Transient reactance"));
                if (generatorCreationInfos.getStepUpTransformerReactance() != null) {
                    shortCircuitReports.add(ModificationUtils.getInstance().buildCreationReport(generatorCreationInfos.getStepUpTransformerReactance(), "Transformer reactance"));
                }
            } catch (PowsyblException e) {
                shortCircuitReports.add(Report.builder()
                        .withKey("ShortCircuitExtensionAddError")
                        .withDefaultMessage("cannot add short-circuit extension on generator with id=${id} : " + e.getMessage())
                        .withValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, shortCircuitReports, "shortCircuitCreated", "Short-circuit");
        }
    }

    private void createGeneratorStartUp(GeneratorCreationInfos generatorCreationInfos, Generator generator, Reporter subReporter) {
        if (generatorCreationInfos.getPlannedActivePowerSetPoint() != null
                || generatorCreationInfos.getStartupCost() != null
                || generatorCreationInfos.getMarginalCost() != null
                || generatorCreationInfos.getPlannedOutageRate() != null
                || generatorCreationInfos.getForcedOutageRate() != null) {
            List<Report> startupReports = new ArrayList<>();
            try {
                generator.newExtension(GeneratorStartupAdderImpl.class)
                        .withPlannedActivePowerSetpoint(nanIfNull(generatorCreationInfos.getPlannedActivePowerSetPoint()))
                        .withStartupCost(nanIfNull(generatorCreationInfos.getStartupCost()))
                        .withMarginalCost(nanIfNull(generatorCreationInfos.getMarginalCost()))
                        .withPlannedOutageRate(nanIfNull(generatorCreationInfos.getPlannedOutageRate()))
                        .withForcedOutageRate(nanIfNull(generatorCreationInfos.getForcedOutageRate()))
                        .add();
                startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getPlannedActivePowerSetPoint(), "Planning active power set point"));
                startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getStartupCost(), "Startup cost"));
                startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getMarginalCost(), "Marginal cost"));
                startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getPlannedOutageRate(), "Planning outage rate"));
                startupReports.add(ModificationUtils.getInstance().buildCreationReport(
                        generatorCreationInfos.getForcedOutageRate(), "Forced outage rate"));
            } catch (PowsyblException e) {
                startupReports.add(Report.builder()
                        .withKey("StartupExtensionAddError")
                        .withDefaultMessage("cannot add startup extension on generator with id=${id} : " + e.getMessage())
                        .withValue("id", generatorCreationInfos.getEquipmentId())
                        .withSeverity(TypedValue.ERROR_SEVERITY)
                        .build());
            }
            ModificationUtils.getInstance().reportModifications(subReporter, startupReports, "startUpAttributesCreated", "Start up");
        }
    }
}
