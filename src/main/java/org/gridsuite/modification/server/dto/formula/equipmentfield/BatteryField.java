package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.MinMaxReactiveLimits;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.modification.server.NetworkModificationException;

public enum BatteryField implements EquipmentField {
    MINIMUM_ACTIVE_POWER,
    MAXIMUM_ACTIVE_POWER,
    ACTIVE_POWER_SET_POINT,
    REACTIVE_POWER_SET_POINT,
    DROOP;

    @Override
    public IdentifiableType getIdentifiableType() {
        return IdentifiableType.BATTERY;
    }

    public static Double getReferenceValue(Battery battery, BatteryField batteryField) {
        ActivePowerControl<Battery> activePowerControl = battery.getExtension(ActivePowerControl.class);
        return switch (batteryField) {
            case MINIMUM_ACTIVE_POWER -> battery.getMinP();
            case MAXIMUM_ACTIVE_POWER -> battery.getMaxP();
            case ACTIVE_POWER_SET_POINT -> battery.getTargetP();
            case REACTIVE_POWER_SET_POINT -> battery.getTargetQ();
            case DROOP -> activePowerControl != null ? activePowerControl.getDroop() : null;
        };
    }

    public static void setNewValue(Battery battery, BatteryField batteryField, Double newValue) {
        switch (batteryField) {
            case MINIMUM_ACTIVE_POWER -> battery.setMinP(newValue);
            case MAXIMUM_ACTIVE_POWER -> battery.setMaxP(newValue);
            case ACTIVE_POWER_SET_POINT -> battery.setTargetP(newValue);
            case REACTIVE_POWER_SET_POINT -> battery.setTargetQ(newValue);
            case DROOP -> battery.newExtension(ActivePowerControlAdder.class)
                    .withDroop(newValue)
                    .add();
        }
    }
}
