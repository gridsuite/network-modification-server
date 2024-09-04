/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.*;

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

    public static Object getReferenceValue(Generator generator, String generatorField) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        CoordinatedReactiveControl coordinatedReactiveControl = generator.getExtension(CoordinatedReactiveControl.class);
        GeneratorField field = GeneratorField.valueOf(generatorField);
        return switch (field) {
            case VOLTAGE_REGULATOR_ON -> generator.isVoltageRegulatorOn();
            case MAXIMUM_ACTIVE_POWER -> generator.getMaxP();
            case MINIMUM_ACTIVE_POWER -> generator.getMinP();
            case ACTIVE_POWER_SET_POINT -> generator.getTargetP();
            case RATED_NOMINAL_POWER -> generator.getRatedS();
            case REACTIVE_POWER_SET_POINT -> generator.getTargetQ();
            case VOLTAGE_SET_POINT -> generator.getTargetV();
            case PLANNED_ACTIVE_POWER_SET_POINT -> generatorStartup != null ? generatorStartup.getPlannedActivePowerSetpoint() : null;
            case MARGINAL_COST -> generatorStartup != null ? generatorStartup.getMarginalCost() : null;
            case PLANNED_OUTAGE_RATE -> generatorStartup != null ? generatorStartup.getPlannedOutageRate() : null;
            case FORCED_OUTAGE_RATE -> generatorStartup != null ? generatorStartup.getForcedOutageRate() : null;
            case DROOP -> activePowerControl != null ? activePowerControl.getDroop() : null;
            case TRANSIENT_REACTANCE -> generatorShortCircuit != null ? generatorShortCircuit.getDirectTransX() : null;
            case STEP_UP_TRANSFORMER_REACTANCE -> generatorShortCircuit != null ? generatorShortCircuit.getStepUpTransformerX() : null;
            case Q_PERCENT -> coordinatedReactiveControl != null ? coordinatedReactiveControl.getQPercent() : null;
        };
    }

    public static <T> void setNewValue(Generator generator, String generatorField, T newValue) {
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        GeneratorField field = GeneratorField.valueOf(generatorField);
        switch (field) {
            case MAXIMUM_ACTIVE_POWER -> generator.setMaxP((double) newValue);
            case MINIMUM_ACTIVE_POWER -> generator.setMinP((double) newValue);
            case ACTIVE_POWER_SET_POINT -> generator.setTargetP((double) newValue);
            case RATED_NOMINAL_POWER -> generator.setRatedS((double) newValue);
            case REACTIVE_POWER_SET_POINT -> generator.setTargetQ((double) newValue);
            case VOLTAGE_SET_POINT -> generator.setTargetV((double) newValue);
            case PLANNED_ACTIVE_POWER_SET_POINT -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withPlannedActivePowerSetpoint((double) newValue)
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint((double) newValue)
                            .withPlannedOutageRate(generatorStartup.getPlannedOutageRate())
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case MARGINAL_COST -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost((double) newValue)
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost((double) newValue)
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate(generatorStartup.getPlannedOutageRate())
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case PLANNED_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withPlannedOutageRate((double) newValue)
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate((double) newValue)
                            .withForcedOutageRate(generatorStartup.getForcedOutageRate())
                            .add();
                }
            }
            case FORCED_OUTAGE_RATE -> {
                if (generatorStartup == null) {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withForcedOutageRate((double) newValue)
                            .add();
                } else {
                    generator.newExtension(GeneratorStartupAdder.class)
                            .withMarginalCost(generatorStartup.getMarginalCost())
                            .withPlannedActivePowerSetpoint(generatorStartup.getPlannedActivePowerSetpoint())
                            .withPlannedOutageRate(generatorStartup.getForcedOutageRate())
                            .withForcedOutageRate((double) newValue)
                            .add();
                }
            }
            case DROOP -> generator.newExtension(ActivePowerControlAdder.class)
                    .withDroop((double) newValue)
                    .add();
            case TRANSIENT_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX((double) newValue)
                    .withStepUpTransformerX(generatorShortCircuit == null ? Double.NaN : generatorShortCircuit.getStepUpTransformerX())
                    .add();
            case STEP_UP_TRANSFORMER_REACTANCE -> generator.newExtension(GeneratorShortCircuitAdder.class)
                    .withDirectTransX(generatorShortCircuit == null ? 0.0D : generatorShortCircuit.getDirectTransX())
                    .withStepUpTransformerX((double) newValue)
                    .add();
            case Q_PERCENT -> generator.newExtension(CoordinatedReactiveControlAdder.class)
                    .withQPercent((double) newValue)
                    .add();
            case VOLTAGE_REGULATOR_ON -> generator.setVoltageRegulatorOn((boolean) newValue);
        }
    }
}
