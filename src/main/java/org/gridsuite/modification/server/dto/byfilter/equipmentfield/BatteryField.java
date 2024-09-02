/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;

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
            case MINIMUM_ACTIVE_POWER -> battery.setMinP(newValue);
            case MAXIMUM_ACTIVE_POWER -> battery.setMaxP(newValue);
            case ACTIVE_POWER_SET_POINT -> battery.setTargetP(newValue);
            case REACTIVE_POWER_SET_POINT -> battery.setTargetQ(newValue);
            case DROOP -> battery.newExtension(ActivePowerControlAdder.class)
                    .withDroop(newValue)
                    .add();
        }
    }

    public static void setNewValue(Battery battery, AbstractSimpleModificationByFilterInfos<?> modificationByFilterInfos) {
        switch (modificationByFilterInfos.getDataType()) {
            case DOUBLE -> setNewValue(battery, modificationByFilterInfos.getEditedField(), (Double) modificationByFilterInfos.getValue());
        }
    }
}
