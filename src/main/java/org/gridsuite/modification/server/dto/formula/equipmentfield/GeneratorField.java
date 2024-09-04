/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import com.powsybl.iidm.network.extensions.CoordinatedReactiveControl;
import com.powsybl.iidm.network.extensions.GeneratorShortCircuit;
import com.powsybl.iidm.network.extensions.GeneratorStartup;
import com.powsybl.network.store.iidm.impl.extensions.CoordinatedReactiveControlAdderImpl;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.modifications.ModificationUtils;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
import static org.gridsuite.modification.server.modifications.GeneratorModification.modifyGeneratorActiveLimitsAttributes;
import static org.gridsuite.modification.server.modifications.GeneratorModification.modifyGeneratorShortCircuitAttributes;
import static org.gridsuite.modification.server.modifications.GeneratorModification.modifyGeneratorStartUpAttributes;
import static org.gridsuite.modification.server.modifications.GeneratorModification.modifyTargetQ;
import static org.gridsuite.modification.server.modifications.GeneratorModification.modifyTargetV;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum GeneratorField {
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

    public static Double getReferenceValue(Generator generator, String generatorField) {
        ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
        GeneratorStartup generatorStartup = generator.getExtension(GeneratorStartup.class);
        GeneratorShortCircuit generatorShortCircuit = generator.getExtension(GeneratorShortCircuit.class);
        CoordinatedReactiveControl coordinatedReactiveControl = generator.getExtension(CoordinatedReactiveControl.class);
        GeneratorField field = GeneratorField.valueOf(generatorField);
        return switch (field) {
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

    public static void setNewValue(Generator generator, String generatorField, Double newValue) {
        if (!Double.isNaN(newValue)) {
            GeneratorField field = GeneratorField.valueOf(generatorField);
            final AttributeModification<Double> attrModif = new AttributeModification<>(newValue, OperationType.SET);
            switch (field) {
                case MAXIMUM_ACTIVE_POWER -> modifyGeneratorActiveLimitsAttributes(
                        attrModif, null, null, generator, null);
                case MINIMUM_ACTIVE_POWER -> modifyGeneratorActiveLimitsAttributes(null, attrModif, null, generator, null);
                case ACTIVE_POWER_SET_POINT -> {
                    ModificationUtils.getInstance().checkActivePowerZeroOrBetweenMinAndMaxActivePower(
                            attrModif, null, null,
                            generator.getMinP(), generator.getMaxP(), generator.getTargetP(),
                            MODIFY_GENERATOR_ERROR, "Generator '" + generator.getId() + "' : "
                    );
                    generator.setTargetP(newValue);
                }
                case RATED_NOMINAL_POWER -> modifyGeneratorActiveLimitsAttributes(null, null, attrModif, generator, null);
                case REACTIVE_POWER_SET_POINT -> modifyTargetQ(generator, attrModif);
                case VOLTAGE_SET_POINT -> modifyTargetV(generator, attrModif);
                case PLANNED_ACTIVE_POWER_SET_POINT ->
                    modifyGeneratorStartUpAttributes(attrModif, null, null, null, generator, null, null);
                case MARGINAL_COST ->
                        modifyGeneratorStartUpAttributes(null, attrModif, null, null, generator, null, null);
                case PLANNED_OUTAGE_RATE ->
                        modifyGeneratorStartUpAttributes(null, null, attrModif, null, generator, null, null);
                case FORCED_OUTAGE_RATE ->
                        modifyGeneratorStartUpAttributes(null, null, null, attrModif, generator, null, null);
                case DROOP -> {
                    ActivePowerControl<Generator> activePowerControl = generator.getExtension(ActivePowerControl.class);
                    ActivePowerControlAdder<Generator> activePowerControlAdder = generator.newExtension(ActivePowerControlAdder.class);
                    ModificationUtils.getInstance().modifyActivePowerControlAttributes(activePowerControl, activePowerControlAdder, null,
                            new AttributeModification<>(newValue.floatValue(), OperationType.SET), null, null);
                }
                case TRANSIENT_REACTANCE -> modifyGeneratorShortCircuitAttributes(attrModif, null, generator, null);
                case STEP_UP_TRANSFORMER_REACTANCE -> modifyGeneratorShortCircuitAttributes(null, attrModif, generator, null);
                case Q_PERCENT -> generator.newExtension(CoordinatedReactiveControlAdderImpl.class)
                        .withQPercent(newValue)
                        .add();
            }
        }
    }
}
