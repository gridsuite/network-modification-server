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

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum BatteryField {
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    DROOP;

    public static String getReferenceValue(Battery battery, String batteryField) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        BatteryField field = BatteryField.valueOf(batteryField);
        return switch (field) {
            case MINIMUM_ACTIVE_POWER -> String.valueOf(battery.getMinP());
            case MAXIMUM_ACTIVE_POWER -> String.valueOf(battery.getMaxP());
            case ACTIVE_POWER_SET_POINT -> String.valueOf(battery.getTargetP());
            case REACTIVE_POWER_SET_POINT -> String.valueOf(battery.getTargetQ());
            case DROOP -> activePowerControl != null ? String.valueOf(activePowerControl.getDroop()) : null;
        };
    }

    public static void setNewValue(Battery battery, String batteryField, String newValue) {
        BatteryField field = BatteryField.valueOf(batteryField);
        switch (field) {
            case MINIMUM_ACTIVE_POWER -> battery.setMinP(Double.parseDouble(newValue));
            case MAXIMUM_ACTIVE_POWER -> battery.setMaxP(Double.parseDouble(newValue));
            case ACTIVE_POWER_SET_POINT -> battery.setTargetP(Double.parseDouble(newValue));
            case REACTIVE_POWER_SET_POINT -> battery.setTargetQ(Double.parseDouble(newValue));
            case DROOP -> battery.newExtension(ActivePowerControlAdder.class)
                    .withDroop(Double.parseDouble(newValue))
                    .add();
        }
    }
}
