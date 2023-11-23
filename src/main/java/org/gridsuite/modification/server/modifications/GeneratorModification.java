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
import com.powsybl.iidm.network.*;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorModification extends AbstractModification {

    private static final String LIMITS = "Limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";

    private final GeneratorModificationInfos modificationInfos;

    public GeneratorModification(GeneratorModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
    }

    @Override
    public void check(Network network) throws NetworkModificationException {
        if (network.getGenerator(modificationInfos.getEquipmentId()) != null) {
            Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
            // check min max reactive limits
            String errorMessage = "Generator '" + modificationInfos.getEquipmentId() + "' : ";
            if (generator.getReactiveLimits().getKind() == ReactiveLimitsKind.MIN_MAX && (modificationInfos.getMinimumReactivePower() != null || modificationInfos.getMaximumReactivePower() != null)) {
                MinMaxReactiveLimits minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimits.class);
                ModificationUtils.getInstance().checkMaxReactivePowerGreaterThanMinReactivePower(minMaxReactiveLimits, modificationInfos.getMinimumReactivePower(), modificationInfos.getMaximumReactivePower(), MODIFY_GENERATOR_ERROR, errorMessage);
            }
            // check reactive capability curve limits
            Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
            List<ReactiveCapabilityCurve.Point> generatorPoints = new ArrayList<>(points);
            List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
            if (!CollectionUtils.isEmpty(points) && modificationPoints != null) {
                ModificationUtils.getInstance().checkMaxQGreaterThanMinQ(generatorPoints, modificationPoints, MODIFY_GENERATOR_ERROR, errorMessage);
            }
            // check regulated terminal
            if (modificationInfos.getRegulatingTerminalId() != null && modificationInfos.getRegulatingTerminalType() != null &&
                    modificationInfos.getRegulatingTerminalVlId() != null) {
                VoltageLevel voltageLevel = ModificationUtils.getInstance().getVoltageLevel(network, modificationInfos.getRegulatingTerminalVlId().getValue());
                ModificationUtils.getInstance().getTerminalFromIdentifiable(voltageLevel.getNetwork(),
                        modificationInfos.getRegulatingTerminalId().getValue(),
                        modificationInfos.getRegulatingTerminalType().getValue(),
                        modificationInfos.getRegulatingTerminalVlId().getValue());
            }
            checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(modificationInfos, generator, MODIFY_GENERATOR_ERROR, errorMessage);
        }
    }

    public void checkActivePowerZeroOrBetweenMinAndMaxActivePowerGenerator(GeneratorModificationInfos modificationInfos, Generator generator, NetworkModificationException.Type exceptionType, String errorMessage) {
        ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                modificationInfos.getActivePowerSetpoint(),
                modificationInfos.getMinActivePower(),
                modificationInfos.getMaxActivePower(),
                generator.getMinP(),
                generator.getMaxP(),
                generator.getTargetP(),
                exceptionType,
                errorMessage
        );
    }

    @Override
    public void apply(Network network, Reporter subReporter) {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MODIFY_GENERATOR_ERROR, "Missing required attributes to modify the equipment");
        }
        Generator generator = ModificationUtils.getInstance().getGenerator(network, modificationInfos.getEquipmentId());
        // modify the generator in the network
        modifyGenerator(generator, modificationInfos, subReporter);
    }

    private void modifyGenerator(Generator generator, GeneratorModificationInfos modificationInfos, Reporter subReporter) {
        subReporter.report(Report.builder()
                .withKey("generatorModification")
                .withDefaultMessage("Generator with id=${id} modified :")
                .withValue("id", modificationInfos.getEquipmentId())
                .withSeverity(TypedValue.INFO_SEVERITY)
                .build());

        if (modificationInfos.getEquipmentName() != null && modificationInfos.getEquipmentName().getValue() != null) {
            ModificationUtils.getInstance().applyElementaryModifications(generator::setName, () -> generator.getOptionalName().orElse("No value"), modificationInfos.getEquipmentName(), subReporter, "Name");
        }
        ModificationUtils.getInstance().applyElementaryModifications(generator::setEnergySource, generator::getEnergySource, modificationInfos.getEnergySource(), subReporter, "Energy source");

        modifyGeneratorLimitsAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorSetpointsAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorShortCircuitAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorStartUpAttributes(modificationInfos, generator, subReporter);
    }

    private void modifyGeneratorShortCircuitAttributes(GeneratorModificationInfos modificationInfos,
                                                       Generator generator, Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        Double oldTransientReactance = generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : Double.NaN;
        Double oldStepUpTransformerReactance = generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : Double.NaN;
        // Either transient reactance or step-up transformer reactance are modified or
        // both
        if (modificationInfos.getTransientReactance() != null
                && modificationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(modificationInfos.getTransientReactance().getValue())
                    .withStepUpTransformerX(modificationInfos.getStepUpTransformerReactance().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldTransientReactance,
                    modificationInfos.getTransientReactance().getValue(),
                    "Transient reactance"));
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldStepUpTransformerReactance,
                    modificationInfos.getStepUpTransformerReactance().getValue(),
                    "Transformer reactance"));

        } else if (modificationInfos.getTransientReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(modificationInfos.getTransientReactance().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldTransientReactance,
                    modificationInfos.getTransientReactance().getValue(),
                    "Transient reactance"));
        } else if (modificationInfos.getStepUpTransformerReactance() != null) {
            generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withStepUpTransformerX(modificationInfos.getStepUpTransformerReactance().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldStepUpTransformerReactance,
                    modificationInfos.getStepUpTransformerReactance().getValue(),
                    "Transformer reactance"));
        }
        ModificationUtils.getInstance().reportModifications(subReporter, reports, "shortCircuitAttributesModified", "Short-circuit");
    }

    private void modifyGeneratorMinMaxReactiveLimits(GeneratorModificationInfos modificationInfos, Generator generator,
                                                     Reporter subReporter, Reporter subReporterLimits) {
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = generator.getReactiveLimits();
        MinMaxReactiveLimitsAdder newMinMaxReactiveLimitsAdder = generator.newMinMaxReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }
        ModificationUtils.getInstance().modifyMinMaxReactiveLimits(minMaxReactiveLimits,
                newMinMaxReactiveLimitsAdder, subReporter, subReporterLimits,
                modificationInfos.getMinimumReactivePower(),
                modificationInfos.getMaximumReactivePower());
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
                                                              Generator generator, Reporter subReporter, Reporter subReporterLimits) {
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();
        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        ModificationUtils.getInstance().modifyReactiveCapabilityCurvePoints(points, modificationPoints, adder, subReporter, subReporterLimits);
    }

    private Reporter modifyGeneratorActiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                           Generator generator, Reporter subReporter) {
        Reporter subReporterLimits = null;

        Report reportMaxActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMaxP, generator::getMaxP, modificationInfos.getMaxActivePower(), "Max active power");
        Report reportMinActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setMinP, generator::getMinP, modificationInfos.getMinActivePower(), "Min active power");
        Report reportRatedNominalPower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setRatedS, generator::getRatedS, modificationInfos.getRatedNominalPower(), "Rated nominal power");
        if (reportMaxActivePower != null || reportMinActivePower != null || reportRatedNominalPower != null) {
            subReporterLimits = subReporter.createSubReporter(LIMITS, LIMITS);
            subReporterLimits.report(Report.builder()
                    .withKey(LIMITS)
                    .withDefaultMessage(LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());

            Reporter subReporterActiveLimits = subReporterLimits.createSubReporter(ACTIVE_LIMITS, ACTIVE_LIMITS);
            subReporterActiveLimits.report(Report.builder()
                    .withKey(ACTIVE_LIMITS)
                    .withDefaultMessage(ACTIVE_LIMITS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (reportMaxActivePower != null) {
                subReporterActiveLimits.report(reportMaxActivePower);
            }
            if (reportMinActivePower != null) {
                subReporterActiveLimits.report(reportMinActivePower);
            }
            if (reportRatedNominalPower != null) {
                subReporterActiveLimits.report(reportRatedNominalPower);
            }
        }
        return subReporterLimits;
    }

    private void modifyGeneratorReactiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                         Generator generator, Reporter subReporter, Reporter subReporterLimits) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (modificationInfos.getReactiveCapabilityCurve() != null) {
            if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                    && modificationInfos.getReactiveCapabilityCurvePoints() != null
                    && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
                modifyGeneratorReactiveCapabilityCurvePoints(modificationInfos, generator, subReporter, subReporterLimits);
            } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
                modifyGeneratorMinMaxReactiveLimits(modificationInfos, generator, subReporter, subReporterLimits);
            }
        }
    }

    private Reporter modifyGeneratorActivePowerControlAttributes(GeneratorModificationInfos modificationInfos,
                                                                 Generator generator, Reporter subReporter, Reporter subReporterSetpoints) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);
        return ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, modificationInfos.getParticipate(), modificationInfos.getDroop(), subReporter, subReporterSetpoints);
    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
                                                  Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorStartupAdder generatorStartupAdder = generator.newExtension(GeneratorStartupAdder.class);
        boolean plannedActivePowerSetPointUpdated = addPlannedActivePowerSetPoint(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean marginalCostUpdated = addMarginalCost(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean plannedOutageRateUpdated = addPlannedOutageRate(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean forcedOutageRateUpdated = addForcedOutageRate(modificationInfos, generatorStartupAdder, generatorStartup, reports);

        if (plannedActivePowerSetPointUpdated ||
                marginalCostUpdated ||
                plannedOutageRateUpdated ||
                forcedOutageRateUpdated) {
            generatorStartupAdder.add();
            ModificationUtils.getInstance().reportModifications(subReporter, reports, "startUpAttributesModified", "Start up");
        }
    }

    private boolean addForcedOutageRate(GeneratorModificationInfos modificationInfos, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<Report> reports) {
        Double oldForcedOutageRate = generatorStartup != null ? generatorStartup.getForcedOutageRate() : Double.NaN;
        if (modificationInfos.getForcedOutageRate() != null) {
            generatorStartupAdder
                    .withForcedOutageRate(modificationInfos.getForcedOutageRate().getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldForcedOutageRate,
                    modificationInfos.getForcedOutageRate().getValue(),
                    "Forced outage rate"));
            return true;
        } else {
            generatorStartupAdder
                    .withForcedOutageRate(oldForcedOutageRate);
        }
        return false;
    }

    private boolean addPlannedOutageRate(GeneratorModificationInfos modificationInfos, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<Report> reports) {
        Double oldPlannedOutageRate = generatorStartup != null ? generatorStartup.getPlannedOutageRate() : Double.NaN;
        if (modificationInfos.getPlannedOutageRate() != null) {
            generatorStartupAdder
                    .withPlannedOutageRate(modificationInfos.getPlannedOutageRate().getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedOutageRate,
                    modificationInfos.getPlannedOutageRate().getValue(),
                    "Planning outage rate"));
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedOutageRate(oldPlannedOutageRate);
        }
        return false;
    }

    private boolean addMarginalCost(GeneratorModificationInfos modificationInfos, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<Report> reports) {
        Double oldMarginalCost = generatorStartup != null ? generatorStartup.getMarginalCost() : Double.NaN;
        if (modificationInfos.getMarginalCost() != null) {
            generatorStartupAdder
                    .withMarginalCost(modificationInfos.getMarginalCost().getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMarginalCost,
                    modificationInfos.getMarginalCost().getValue(),
                    "Marginal cost"));
            return true;
        } else {
            generatorStartupAdder
                    .withMarginalCost(oldMarginalCost);
        }
        return false;
    }

    private boolean addPlannedActivePowerSetPoint(GeneratorModificationInfos modificationInfos, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<Report> reports) {
        Double oldPlannedActivePowerSetPoint = generatorStartup != null ? generatorStartup.getPlannedActivePowerSetpoint() : Double.NaN;
        if (modificationInfos.getPlannedActivePowerSetPoint() != null) {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(modificationInfos.getPlannedActivePowerSetPoint().getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldPlannedActivePowerSetPoint,
                    modificationInfos.getPlannedActivePowerSetPoint().getValue(),
                    "Planning active power set point"));
            return true;
        } else {
            generatorStartupAdder
                    .withPlannedActivePowerSetpoint(oldPlannedActivePowerSetPoint);
        }
        return false;
    }

    private void modifyGeneratorRegulatingTerminal(GeneratorModificationInfos modificationInfos, Generator generator, List<Report> modificationReports) {
        Terminal regulatingTerminal = generator.getRegulatingTerminal();

        String oldVoltageLevel = null;
        String oldEquipment = null;
        // If there is no regulating terminal in file, regulating terminal voltage level
        // is equal to generator voltage level
        if (regulatingTerminal != null
                && !regulatingTerminal.getVoltageLevel().equals(generator.getTerminal().getVoltageLevel())) {
            oldVoltageLevel = regulatingTerminal.getVoltageLevel().getId();
            oldEquipment = regulatingTerminal.getConnectable().getType().name() + ":"
                    + regulatingTerminal.getConnectable().getId();
        }

        if (modificationInfos.getRegulatingTerminalId() != null
                && modificationInfos.getRegulatingTerminalType() != null
                && modificationInfos.getRegulatingTerminalVlId() != null) {
            Terminal terminal = ModificationUtils.getInstance().getTerminalFromIdentifiable(generator.getNetwork(),
                    modificationInfos.getRegulatingTerminalId().getValue(),
                    modificationInfos.getRegulatingTerminalType().getValue(),
                    modificationInfos.getRegulatingTerminalVlId().getValue());
            generator.setRegulatingTerminal(terminal);

            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    modificationInfos.getRegulatingTerminalVlId().getValue(),
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    modificationInfos.getRegulatingTerminalType().getValue() + ":"
                            + modificationInfos.getRegulatingTerminalId().getValue(),
                    "Equipment"));
        }

        // if the voltageRegulationType is set to LOCAL, we set the regulatingTerminal
        // to null
        if (modificationInfos.getVoltageRegulationType() != null
                && modificationInfos.getVoltageRegulationType().getValue() == VoltageRegulationType.LOCAL
                && oldEquipment != null && oldVoltageLevel != null) {
            generator.setRegulatingTerminal(null);
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldVoltageLevel,
                    null,
                    "Voltage level"));
            modificationReports.add(ModificationUtils.getInstance().buildModificationReport(oldEquipment,
                    null,
                    "Equipment"));
        }
    }

    private Reporter modifyGeneratorVoltageRegulatorAttributes(GeneratorModificationInfos modificationInfos,
                                                               Generator generator, Reporter subReporter, Reporter subReporterSetpoints) {
        List<Report> voltageRegulationReports = new ArrayList<>();

        Report reportVoltageSetpoint = null;
        if (modificationInfos.getVoltageSetpoint() != null) {
            if (modificationInfos.getVoltageSetpoint().getOp() == OperationType.SET) {
                reportVoltageSetpoint = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetV, generator::getTargetV,
                    modificationInfos.getVoltageSetpoint(), "Voltage");
            } else {
                reportVoltageSetpoint = ModificationUtils.getInstance().buildModificationReport(generator.getTargetV(), Double.NaN, "Voltage");
            }
        }

        voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                modificationInfos.getVoltageRegulationOn(), "VoltageRegulationOn"));
        if (reportVoltageSetpoint != null) {
            voltageRegulationReports.add(reportVoltageSetpoint);
        }

        // We apply modifications to regulatingTerminal and QPercent
        // we apply modifications to the reactivepower setpoint
        modifyGeneratorRegulatingTerminal(modificationInfos, generator, voltageRegulationReports);
        if (modificationInfos.getQPercent() != null) {
            CoordinatedReactiveControl coordinatedReactiveControl = generator
                    .getExtension(CoordinatedReactiveControl.class);
            Double oldQPercent = coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent()
                    : Double.NaN;
            generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                    .withQPercent(modificationInfos.getQPercent().getValue())
                    .add();
            voltageRegulationReports.add(ModificationUtils.getInstance().buildModificationReport(
                    oldQPercent,
                    modificationInfos.getQPercent().getValue(), "Reactive percentage"));
        }

        //TargetQ and TargetV are unset after voltage regulation have been dealt with otherwise it can cause unwanted validations exceptions
        if (modificationInfos.getVoltageSetpoint() != null && modificationInfos.getVoltageSetpoint().getOp() == OperationType.UNSET) {
            generator.setTargetV(Double.NaN);
        }

        if (modificationInfos.getReactivePowerSetpoint() != null && modificationInfos.getReactivePowerSetpoint().getOp() == OperationType.UNSET) {
            generator.setTargetQ(Double.NaN);
        }

        Reporter subReporterSetpoints2 = subReporterSetpoints;
        if (subReporterSetpoints == null && !voltageRegulationReports.isEmpty()) {
            subReporterSetpoints2 = subReporter.createSubReporter(SETPOINTS, SETPOINTS);
            subReporterSetpoints2.report(Report.builder()
                    .withKey(SETPOINTS)
                    .withDefaultMessage(SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
        }
        ModificationUtils.getInstance().reportModifications(subReporterSetpoints2, voltageRegulationReports, "voltageRegulationModified", "Voltage regulation");
        return subReporterSetpoints2;
    }

    private void modifyGeneratorSetpointsAttributes(GeneratorModificationInfos modificationInfos,
                                                    Generator generator, Reporter subReporter) {
        Report reportActivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetP, generator::getTargetP, modificationInfos.getActivePowerSetpoint(), "Active power");
        Report reportReactivePower = null;
        if (modificationInfos.getReactivePowerSetpoint() != null) {
            if (modificationInfos.getReactivePowerSetpoint().getOp() == OperationType.SET) {
                reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetQ, generator::getTargetQ, modificationInfos.getReactivePowerSetpoint(), "Reactive power");
            } else {
                reportReactivePower = ModificationUtils.getInstance().buildModificationReport(generator.getTargetQ(), Double.NaN, "Reactive power");
            }
        }

        Reporter subReporterSetpoints = null;
        if (reportActivePower != null || reportReactivePower != null) {
            subReporterSetpoints = subReporter.createSubReporter(SETPOINTS, SETPOINTS);
            subReporterSetpoints.report(Report.builder()
                    .withKey(SETPOINTS)
                    .withDefaultMessage(SETPOINTS)
                    .withSeverity(TypedValue.INFO_SEVERITY)
                    .build());
            if (reportActivePower != null) {
                subReporterSetpoints.report(reportActivePower);
            }
            if (reportReactivePower != null) {
                subReporterSetpoints.report(reportReactivePower);
            }
        }
        subReporterSetpoints = modifyGeneratorVoltageRegulatorAttributes(modificationInfos, generator, subReporter, subReporterSetpoints);
        modifyGeneratorActivePowerControlAttributes(modificationInfos, generator, subReporter, subReporterSetpoints);
    }

    private void modifyGeneratorLimitsAttributes(GeneratorModificationInfos modificationInfos,
                                                 Generator generator, Reporter subReporter) {
        Reporter subReporterLimits = modifyGeneratorActiveLimitsAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorReactiveLimitsAttributes(modificationInfos, generator, subReporter, subReporterLimits);
    }
}
