/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.*;
import jakarta.validation.constraints.NotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum GeneratorField {
    VOLTAGE_REGULATOR_ON,
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    RATED_NOMINAL_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    VOLTAGE_SET_POINT,
    PLANNED_ACTIVE_POWER_SET_POINT,
    MARGINAL_COST,
    PLANNED_OUTAGE_RATE,
    FORCED_OUTAGE_RATE,
    DROOP,
    TRANSIENT_REACTANCE,
    STEP_UP_TRANSFORMER_REACTANCE,
    Q_PERCENT;

    public static String getReferenceValue(Generator generator, String generatorField) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        CoordinatedReactiveControl coordinatedReactiveControl = generator.getExtension(CoordinatedReactiveControl.class);
        GeneratorField field = GeneratorField.valueOf(generatorField);
        return switch (field) {
            case VOLTAGE_REGULATOR_ON -> String.valueOf(generator.isVoltageRegulatorOn());
            case MAXIMUM_ACTIVE_POWER -> String.valueOf(generator.getMaxP());
            case MINIMUM_ACTIVE_POWER -> String.valueOf(generator.getMinP());
            case ACTIVE_POWER_SET_POINT -> String.valueOf(generator.getTargetP());
            case RATED_NOMINAL_POWER -> String.valueOf(generator.getRatedS());
            case REACTIVE_POWER_SET_POINT -> String.valueOf(generator.getTargetQ());
            case VOLTAGE_SET_POINT -> String.valueOf(generator.getTargetV());
            case PLANNED_ACTIVE_POWER_SET_POINT -> generatorStartup != null ? String.valueOf(generatorStartup.getPlannedActivePowerSetpoint()) : null;
            case MARGINAL_COST -> generatorStartup != null ? String.valueOf(generatorStartup.getMarginalCost()) : null;
            case PLANNED_OUTAGE_RATE -> generatorStartup != null ? String.valueOf(generatorStartup.getPlannedOutageRate()) : null;
            case FORCED_OUTAGE_RATE -> generatorStartup != null ? String.valueOf(generatorStartup.getForcedOutageRate()) : null;
            case DROOP -> activePowerControl != null ? String.valueOf(activePowerControl.getDroop()) : null;
            case TRANSIENT_REACTANCE -> generatorShortCircuit != null ? String.valueOf(generatorShortCircuit.getDirectTransX()) : null;
            case STEP_UP_TRANSFORMER_REACTANCE -> generatorShortCircuit != null ? String.valueOf(generatorShortCircuit.getStepUpTransformerX()) : null;
            case Q_PERCENT -> coordinatedReactiveControl != null ? String.valueOf(coordinatedReactiveControl.getQPercent()) : null;
        };
    }

    public static void setNewValue(Generator generator, String generatorField, @NotNull String newValue) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        GeneratorField field = GeneratorField.valueOf(generatorField);
        switch (field) {
            case MAXIMUM_ACTIVE_POWER -> generator.setMaxP(Double.parseDouble(newValue));
            case MINIMUM_ACTIVE_POWER -> generator.setMinP(Double.parseDouble(newValue));
            case ACTIVE_POWER_SET_POINT -> generator.setTargetP(Double.parseDouble(newValue));
            case RATED_NOMINAL_POWER -> generator.setRatedS(Double.parseDouble(newValue));
            case REACTIVE_POWER_SET_POINT -> generator.setTargetQ(Double.parseDouble(newValue));
            case VOLTAGE_SET_POINT -> generator.setTargetV(Double.parseDouble(newValue));
            case PLANNED_ACTIVE_POWER_SET_POINT -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withPlannedActivePowerSetpoint(Double.parseDouble(newValue))
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint(Double.parseDouble(newValue))
                            .withPlannedOutageRate(generatorStartup.getPlannedOutageRate())
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case MARGINAL_COST -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(Double.parseDouble(newValue))
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(Double.parseDouble(newValue))
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate(generatorStartup.getPlannedOutageRate())
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case PLANNED_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withPlannedOutageRate(Double.parseDouble(newValue))
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate(Double.parseDouble(newValue))
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case FORCED_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withForcedOutageRate(Double.parseDouble(newValue))
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate(generatorStartup.getForcedOutageRate())
                            .withForcedOutageRate(Double.parseDouble(newValue))
                            .add();
                }
            }
            case DROOP -> generator.newExtension(ActivePowerControlAdder.class)
                    .withDroop(Double.parseDouble(newValue))
                    .add();
            case TRANSIENT_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(Double.parseDouble(newValue))
                    .withStepUpTransformerX(generatorShortCircuit == null ? Double.NaN : generatorShortCircuit.getStepUpTransformerX())
                    .add();
            case STEP_UP_TRANSFORMER_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorShortCircuit == null ? 0.0D : generatorShortCircuit.getDirectTransX())
                    .withStepUpTransformerX(Double.parseDouble(newValue))
                    .add();
            case Q_PERCENT -> generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent(Double.parseDouble(newValue))
                    .add();
            case VOLTAGE_REGULATOR_ON -> generator.setVoltageRegulatorOn(Boolean.parseBoolean(newValue));
        }
    }
}
