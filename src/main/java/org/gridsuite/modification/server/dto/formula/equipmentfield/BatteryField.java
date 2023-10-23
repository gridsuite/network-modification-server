package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.IdentifiableType;

public enum BatteryField implements EquipmentField {
    ACTIVE_POWER;

    @Override
    public IdentifiableType getIdentifiableType() {
        return IdentifiableType.BATTERY;
    }

    public static Double getReferenceValue(Battery battery, BatteryField batteryField) {
        return switch (batteryField) {
            case ACTIVE_POWER -> battery.getMaxP();
        };
    }
}
