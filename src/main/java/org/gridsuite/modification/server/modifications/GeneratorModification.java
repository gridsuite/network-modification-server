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
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorModification extends AbstractModification {

    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";
    private static final String LIMITS = "Limits";
    private static final String REACTIVE_LIMITS = "Reactive limits";
    private static final String ACTIVE_LIMITS = "Active limits";
    private static final String SETPOINTS = "Setpoints";

    private final GeneratorModificationInfos modificationInfos;

    public GeneratorModification(GeneratorModificationInfos modificationInfos) {
        this.modificationInfos = modificationInfos;
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
            ModificationUtils.getInstance().applyElementaryModifications(generator::setName, generator::getNameOrId, modificationInfos.getEquipmentName(), subReporter, "Name");
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
        List<Report> reports = new ArrayList<>();
        // we get previous min max values if they exist
        MinMaxReactiveLimits minMaxReactiveLimits = null;
        ReactiveLimits reactiveLimits = generator.getReactiveLimits();
        if (reactiveLimits != null) {
            ReactiveLimitsKind limitsKind = reactiveLimits.getKind();
            if (limitsKind == ReactiveLimitsKind.MIN_MAX) {
                minMaxReactiveLimits = generator.getReactiveLimits(MinMaxReactiveLimitsImpl.class);
            }
        }

        // (if the min and max reactive limits are null and there is no previous min max
        // limits set we set them to Double max and Double min values)
        // The user can change the value of MinimumReactivePower, MaximumReactivePower or both
        if (modificationInfos.getMinimumReactivePower() != null
                && modificationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (modificationInfos.getMinimumReactivePower() != null) {
            generator.newMinMaxReactiveLimits().setMinQ(modificationInfos.getMinimumReactivePower().getValue())
                    .setMaxQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.MAX_VALUE)
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : Double.NaN,
                    modificationInfos.getMinimumReactivePower().getValue(),
                    MIN_REACTIVE_POWER_FIELDNAME));
        } else if (modificationInfos.getMaximumReactivePower() != null) {
            generator.newMinMaxReactiveLimits()
                    .setMinQ(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMinQ() : -Double.MAX_VALUE)
                    .setMaxQ(modificationInfos.getMaximumReactivePower().getValue())
                    .add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(minMaxReactiveLimits != null ? minMaxReactiveLimits.getMaxQ() : Double.NaN,
                    modificationInfos.getMaximumReactivePower().getValue(),
                    MAX_REACTIVE_POWER_FIELDNAME));
        } else if (minMaxReactiveLimits == null) {
            generator.newMinMaxReactiveLimits().setMinQ(-Double.MAX_VALUE)
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
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, reports, "minMaxReactiveLimitsModified", "By range");
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
                                                              Generator generator, Reporter subReporter, Reporter subReporterLimits) {
        List<Report> reports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();

        List<ReactiveCapabilityCurveModificationInfos> modificationPoints = modificationInfos.getReactiveCapabilityCurvePoints();

        Collection<ReactiveCapabilityCurve.Point> points = generator.getReactiveLimits().getKind() == ReactiveLimitsKind.CURVE ? generator.getReactiveLimits(ReactiveCapabilityCurve.class).getPoints() : List.of();
        List<ReactiveCapabilityCurve.Point> generatorPoints = new ArrayList<>(points);

        IntStream.range(0, modificationPoints.size())
                .forEach(i -> {
                    String fieldSuffix;
                    ReactiveCapabilityCurve.Point oldPoint = i < generatorPoints.size() - 1 ? generatorPoints.get(i) : null;
                    ReactiveCapabilityCurveModificationInfos newPoint = modificationPoints.get(i);
                    if (i == 0) {
                        fieldSuffix = "min";
                    } else if (i == (modificationPoints.size() - 1)) {
                        fieldSuffix = "max";
                        if (!CollectionUtils.isEmpty(generatorPoints)) {
                            oldPoint = generatorPoints.get(generatorPoints.size() - 1);
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
        ModificationUtils.getInstance().reportModifications(subReporterReactiveLimits, reports, "curveReactiveLimitsModified", "By diagram");
    }

    private void createReactiveCapabilityCurvePoint(ReactiveCapabilityCurveAdder adder,
                                                    ReactiveCapabilityCurveModificationInfos newPoint,
                                                    ReactiveCapabilityCurve.Point oldPoint,
                                                    List<Report> reports,
                                                    String fieldSuffix) {
        Double oldMaxQ = null;
        Double oldMinQ = null;
        Double oldP = null;

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

    private void addToReports(List<Report> reports, Double newValue, Double oldValue, String fieldName) {
        if (newValue != null) {
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldValue,
                    newValue,
                    fieldName));
        }
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
        List<Report> reports = new ArrayList<>();

        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        double oldDroop = activePowerControl != null ? activePowerControl.getDroop() : Double.NaN;
        Boolean participate = null;
        // if participate is null and droop was modified, we consider that participate
        // is true
        if (modificationInfos.getParticipate() != null) {
            participate = modificationInfos.getParticipate().getValue();
            reports.add(ModificationUtils.getInstance().buildModificationReport(activePowerControl != null ? activePowerControl.isParticipate() : null,
                    participate,
                    "ON/OFF"));
        } else if (modificationInfos.getDroop() != null) {
            participate = true;
        }
        // if no modification were done to ActivePowerControl or if neither the old nor the new droop values are valid,
        // we don't apply modifications
        if (participate != null) {
            if (Boolean.TRUE.equals(participate)) {
                if (modificationInfos.getDroop() != null) {
                    generator.newExtension(ActivePowerControlAdder.class)
                            .withParticipate(true)
                            .withDroop(modificationInfos.getDroop().getValue())
                            .add();
                    reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop,
                            modificationInfos.getDroop().getValue(),
                            "Droop"));
                } else {
                    generator.newExtension(ActivePowerControlAdder.class)
                            .withParticipate(true).withDroop(oldDroop)
                            .add();
                }
            } else {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(participate).add();
            }
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

        ModificationUtils.getInstance().reportModifications(subReporterSetpoints2, reports, "activePowerRegulationModified", "Active power regulation");
        return subReporterSetpoints2;
    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
                                                  Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorStartupAdder generatorStartupAdder = generator.newExtension(GeneratorStartupAdder.class);
        boolean plannedActivePowerSetPointUpdated = addPlannedActivePowerSetPoint(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean startupCostUpdated = addStartupCost(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean marginalCostUpdated = addMarginalCost(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean plannedOutageRateUpdated = addPlannedOutageRate(modificationInfos, generatorStartupAdder, generatorStartup, reports);
        boolean forcedOutageRateUpdated = addForcedOutageRate(modificationInfos, generatorStartupAdder, generatorStartup, reports);

        if (plannedActivePowerSetPointUpdated ||
                startupCostUpdated ||
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

    private boolean addStartupCost(GeneratorModificationInfos modificationInfos, GeneratorStartupAdder generatorStartupAdder, GeneratorStartup generatorStartup, List<Report> reports) {
        Double oldStartupCost = generatorStartup != null ? generatorStartup.getStartupCost() : Double.NaN;
        if (modificationInfos.getStartupCost() != null) {
            generatorStartupAdder
                    .withStartupCost(modificationInfos.getStartupCost().getValue());
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldStartupCost,
                    modificationInfos.getStartupCost().getValue(),
                    "Startup cost"));
            return true;
        } else {
            generatorStartupAdder
                    .withStartupCost(oldStartupCost);
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

        Report reportVoltageSetpoint = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetV, generator::getTargetV,
            modificationInfos.getVoltageSetpoint(), "Voltage");
        // if no modification were done to VoltageRegulatorOn, we get the old value
        Boolean isVoltageRegulationOn = null;
        if (modificationInfos.getVoltageRegulationOn() != null) {
            isVoltageRegulationOn = modificationInfos.getVoltageRegulationOn().getValue();
            voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                    modificationInfos.getVoltageRegulationOn(), "ON/OFF"));
        } else {
            isVoltageRegulationOn = generator.isVoltageRegulatorOn();
        }
        if (reportVoltageSetpoint != null) {
            voltageRegulationReports.add(reportVoltageSetpoint);
        }

        // if voltageRegulationOn is true, we apply modifications to regulatingTerminal
        // and QPercent
        // otherwise we apply modifications to the reactivepower setpoint
        if (Boolean.TRUE.equals(isVoltageRegulationOn)) {
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
        Report reportReactivePower = ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetQ, generator::getTargetQ, modificationInfos.getReactivePowerSetpoint(), "Reactive power");

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
