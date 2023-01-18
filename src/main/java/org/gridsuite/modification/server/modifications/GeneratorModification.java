/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import org.gridsuite.modification.server.NetworkModificationException;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;

import com.powsybl.commons.reporter.Report;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.TypedValue;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.MinMaxReactiveLimits;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.ReactiveCapabilityCurveAdder;
import com.powsybl.iidm.network.ReactiveLimits;
import com.powsybl.iidm.network.ReactiveLimitsKind;
import com.powsybl.iidm.network.Terminal;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControl;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuitAdder;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.iidm.network.extensions.GeneratorStartupAdder;
import com.powsybl.network.store.iidm.impl.MinMaxReactiveLimitsImpl;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */
public class GeneratorModification extends AbstractModification {

    private static final String MIN_REACTIVE_POWER_FIELDNAME = "Minimum reactive power";
    private static final String MAX_REACTIVE_POWER_FIELDNAME = "Maximum reactive power";

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
        ModificationUtils.getInstance().applyElementaryModifications(generator::setMaxP, generator::getMaxP, modificationInfos.getMaxActivePower(), subReporter, "Max active power");
        ModificationUtils.getInstance().applyElementaryModifications(generator::setMinP, generator::getMinP, modificationInfos.getMinActivePower(), subReporter, "Min active power");
        ModificationUtils.getInstance().applyElementaryModifications(generator::setRatedS, generator::getRatedS, modificationInfos.getRatedNominalPower(), subReporter, "Rated nominal power");
        ModificationUtils.getInstance().applyElementaryModifications(generator::setTargetP, generator::getTargetP, modificationInfos.getActivePowerSetpoint(), subReporter, "Active power set point");
        modifyGeneratorVoltageRegulatorAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorShortCircuitAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorActivePowerControlAttributes(modificationInfos, generator, subReporter);
        modifyGeneratorReactiveLimitsAttributes(modificationInfos, generator, subReporter);
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
        ModificationUtils.getInstance().reportModifications(subReporter, reports, "shortCircuitAttributesModified", "Short-circuit modified :");
    }

    private void modifyGeneratorMinMaxReactiveLimits(GeneratorModificationInfos modificationInfos, Generator generator,
            Reporter subReporter) {
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

        ModificationUtils.getInstance().reportModifications(subReporter, reports, "minMaxReactiveLimitsModified", "Reactive limits By range modified :");
    }

    private void modifyGeneratorReactiveCapabilityCurvePoints(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        ReactiveCapabilityCurveAdder adder = generator.newReactiveCapabilityCurve();
        List<ReactiveCapabilityCurveModificationInfos> points = modificationInfos.getReactiveCapabilityCurvePoints();
        IntStream.range(0, points.size())
                .forEach(i -> {
                    ReactiveCapabilityCurveModificationInfos point = points.get(i);
                    adder.beginPoint()
                            .setMaxQ(point.getQmaxP() != null ? point.getQmaxP() : point.getOldQmaxP())
                            .setMinQ(point.getQminP() != null ? point.getQminP() : point.getOldQminP())
                            .setP(point.getP() != null ? point.getP() : point.getOldP())
                            .endPoint();
                    if (point.getP() != null) {
                        reports.add(ModificationUtils.getInstance().buildModificationReport(point.getOldP(),
                                point.getP(),
                                "P" + i));
                    }
                    if (point.getQminP() != null) {
                        reports.add(ModificationUtils.getInstance().buildModificationReport(point.getOldQminP(),
                                point.getQminP(),
                                "QminP" + i));
                    }
                    if (point.getQmaxP() != null) {
                        reports.add(ModificationUtils.getInstance().buildModificationReport(point.getOldQmaxP(),
                                point.getQmaxP(),
                                "QmaxP" + i));
                    }
                });
        adder.add();
        ModificationUtils.getInstance().reportModifications(subReporter, reports, "curveReactiveLimitsModified", "Reactive limits By diagram modified :");
    }

    private void modifyGeneratorReactiveLimitsAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        // if reactive capability curve is true and there was modifications on the
        // reactive capability curve points,
        // then we have to apply the reactive capability curve modifications
        // else if reactive capability curve is false we have to apply the min and max
        // reactive limits modifications
        if (Boolean.TRUE.equals(modificationInfos.getReactiveCapabilityCurve().getValue()
                && modificationInfos.getReactiveCapabilityCurvePoints() != null
                && !modificationInfos.getReactiveCapabilityCurvePoints().isEmpty())) {
            modifyGeneratorReactiveCapabilityCurvePoints(modificationInfos, generator, subReporter);
        } else if (Boolean.FALSE.equals(modificationInfos.getReactiveCapabilityCurve().getValue())) {
            modifyGeneratorMinMaxReactiveLimits(modificationInfos, generator, subReporter);
        }
    }

    private void modifyGeneratorActivePowerControlAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        Float oldDroop = activePowerControl != null ? activePowerControl.getDroop() : Float.NaN;
        Boolean participate = null;
        // if participate is null and droop was modified, we consider that participate
        // is true
        if (modificationInfos.getParticipate() != null) {
            participate = modificationInfos.getParticipate().getValue();
            reports.add(ModificationUtils.getInstance().buildModificationReport(activePowerControl != null ? activePowerControl.isParticipate() : null,
                    participate,
                    "Active power regulation"));
        } else if (modificationInfos.getDroop() != null) {
            participate = true;
        }
        // if no modification were done to ActivePowerControl or if neither the old nor the new droop values are valid,
        // we don't apply modifications
        if (participate != null) {
            if (Boolean.TRUE.equals(participate)) {
                Float newDroop = modificationInfos.getDroop() != null ? modificationInfos.getDroop().getValue() : oldDroop;
                if (!newDroop.isNaN()) {
                    generator.newExtension(ActivePowerControlAdder.class)
                            .withParticipate(true).withDroop(newDroop)
                            .add();
                    reports.add(ModificationUtils.getInstance().buildModificationReport(oldDroop,
                            newDroop,
                            "Droop"));
                }
            } else {
                generator.newExtension(ActivePowerControlAdder.class)
                        .withParticipate(false).add();
            }
        }
        ModificationUtils.getInstance().reportModifications(subReporter, reports, "activePowerRegulationModified", "Active power regulation modified :");
    }

    private void modifyGeneratorStartUpAttributes(GeneratorModificationInfos modificationInfos, Generator generator,
            Reporter subReporter) {
        List<Report> reports = new ArrayList<>();
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        Double oldMarginalCost = generatorStartup != null ? generatorStartup.getMarginalCost() : Double.NaN;
        if (modificationInfos.getMarginalCost() != null) {
            generator.newExtension(GeneratorStartupAdder.class)
                    .withMarginalCost(modificationInfos.getMarginalCost().getValue()).add();
            reports.add(ModificationUtils.getInstance().buildModificationReport(oldMarginalCost,
                    modificationInfos.getMarginalCost().getValue(),
                    "Cost of start"));
        }

        ModificationUtils.getInstance().reportModifications(subReporter, reports, "startUpAttributesModified", "Start up modified :");
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

    private void modifyGeneratorVoltageRegulatorAttributes(GeneratorModificationInfos modificationInfos,
            Generator generator, Reporter subReporter) {
        List<Report> voltageRegulationReports = new ArrayList<>();

        voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetV, generator::getTargetV,
                modificationInfos.getVoltageSetpoint(), "Voltage set point"));
        // if no modification were done to VoltageRegulatorOn, we get the old value
        Boolean isVoltageRegulationOn = null;
        if (modificationInfos.getVoltageRegulationOn() != null) {
            isVoltageRegulationOn = modificationInfos.getVoltageRegulationOn().getValue();
            voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setVoltageRegulatorOn, generator::isVoltageRegulatorOn,
                    modificationInfos.getVoltageRegulationOn(), "Voltage regulation on"));
        } else {
            isVoltageRegulationOn = generator.isVoltageRegulatorOn();
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
        } else {
            if (modificationInfos.getReactivePowerSetpoint() != null) {
                voltageRegulationReports.add(ModificationUtils.getInstance().applyElementaryModificationsAndReturnReport(generator::setTargetQ, generator::getTargetQ,
                        modificationInfos.getReactivePowerSetpoint(),
                        "Reactive power set point"));
            }
        }
        ModificationUtils.getInstance().reportModifications(subReporter, voltageRegulationReports, "voltageRegulationModified", "Voltage regulation modified :");
    }
}
