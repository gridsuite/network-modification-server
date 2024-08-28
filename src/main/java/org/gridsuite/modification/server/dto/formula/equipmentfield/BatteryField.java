/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.modifications.ModificationUtils;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum BatteryField {
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    DROOP;

    public static Double getReferenceValue(Battery battery, String batteryField) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        BatteryField field = BatteryField.valueOf(batteryField);
        return switch (field) {
            case MINIMUM_ACTIVE_POWER -> battery.getMinP();
            case MAXIMUM_ACTIVE_POWER -> battery.getMaxP();
            case ACTIVE_POWER_SET_POINT -> battery.getTargetP();
            case REACTIVE_POWER_SET_POINT -> battery.getTargetQ();
            case DROOP -> activePowerControl != null ? activePowerControl.getDroop() : null;
        };
    }

    public static void setNewValue(Battery battery, String batteryField, Double newValue) {
        BatteryField field = BatteryField.valueOf(batteryField);
        switch (field) {
            case MINIMUM_ACTIVE_POWER ->
                    ModificationUtils.getInstance().applyElementaryModifications(
                            battery::setMinP,
                            battery::getMinP,
                            new AttributeModification<>(newValue, OperationType.SET));
            case MAXIMUM_ACTIVE_POWER ->
                    ModificationUtils.getInstance().applyElementaryModifications(
                            battery::setMaxP,
                            battery::getMaxP,
                            new AttributeModification<>(newValue, OperationType.SET));
            case ACTIVE_POWER_SET_POINT ->
                ModificationUtils.getInstance().applyElementaryModifications(
                        battery::setTargetP,
                        battery::getTargetP,
                        new AttributeModification<>(newValue, OperationType.SET));
            case REACTIVE_POWER_SET_POINT ->
                ModificationUtils.getInstance().applyElementaryModifications(
                        battery::setTargetQ,
                        battery::getTargetQ,
                        new AttributeModification<>(newValue, OperationType.SET));
            case DROOP -> {
                ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
                ActivePowerControlAdder<Battery> activePowerControlAdder = battery.newExtension(ActivePowerControlAdder.class);
                ModificationUtils.getInstance().modifyActivePowerControlAttributes(
                        activePowerControl,
                        activePowerControlAdder,
                        null,
                        new AttributeModification<>(newValue.floatValue(), OperationType.SET),
                        null,
                        null);
                /* Correction basique juste au cas où je ne fais pas le méga refacto :
                if (battery.getExtension(ActivePowerControl.class) != null) {
                    battery.getExtension(ActivePowerControl.class).setDroop(newValue);
                } else {
                    battery.newExtension(ActivePowerControlAdder.class)
                            .withDroop(newValue)
                            .add();
                }*/
            }
        }
    }
}
