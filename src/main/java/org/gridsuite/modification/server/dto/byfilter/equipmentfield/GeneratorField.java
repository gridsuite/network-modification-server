/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.*;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.modifications.ModificationUtils;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.server.modifications.GeneratorModification.*;

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
        GeneratorField field = GeneratorField.valueOf(generatorField);
        final AttributeModification<Double> attributeModification = new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET);
        switch (field) {
            case MAXIMUM_ACTIVE_POWER -> modifyGeneratorActiveLimitsAttributes(
                    attributeModification, null, null, generator, null);
            case MINIMUM_ACTIVE_POWER -> modifyGeneratorActiveLimitsAttributes(null, attributeModification, null, generator, null);
            case ACTIVE_POWER_SET_POINT -> {
                ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                        attributeModification, null, null,
                        generator.getMinP(), generator.getMaxP(), generator.getTargetP(),
                        MODIFY_GENERATOR_ERROR, "Generator '" + generator.getId() + "' : "
                );
                generator.setTargetP(Double.parseDouble(newValue));
            }
            case RATED_NOMINAL_POWER -> modifyGeneratorActiveLimitsAttributes(null, null, attributeModification, generator, null);
            case REACTIVE_POWER_SET_POINT -> modifyTargetQ(generator, attributeModification);
            case VOLTAGE_SET_POINT -> modifyTargetV(generator, attributeModification);
            case PLANNED_ACTIVE_POWER_SET_POINT ->
                    modifyGeneratorStartUpAttributes(attributeModification, null, null, null, generator, null, null);
            case MARGINAL_COST ->
                    modifyGeneratorStartUpAttributes(null, attributeModification, null, null, generator, null, null);
            case PLANNED_OUTAGE_RATE ->
                    modifyGeneratorStartUpAttributes(null, null, attributeModification, null, generator, null, null);
            case FORCED_OUTAGE_RATE ->
                    modifyGeneratorStartUpAttributes(null, null, null, attributeModification, generator, null, null);
            case DROOP -> {
                ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
                ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);
                ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, null,
                        new AttributeModification<>(Float.parseFloat(newValue), OperationType.SET), null, null);
            }
            case TRANSIENT_REACTANCE -> modifyGeneratorShortCircuitAttributes(attributeModification, null, generator, null);
            case STEP_UP_TRANSFORMER_REACTANCE -> modifyGeneratorShortCircuitAttributes(null, attributeModification, generator, null);
            case Q_PERCENT -> generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                    .withQPercent(Double.parseDouble(newValue))
                    .add();
            case VOLTAGE_REGULATOR_ON -> generator.setVoltageRegulatorOn(Boolean.parseBoolean(newValue));
        }
    }
}
