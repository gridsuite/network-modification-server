package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringEquipmentAttributeModificationEntity;

import com.powsybl.commons.PowsyblException;

public class EquipmentAttributeModificationMapper implements EntityMapper<EquipmentAttributeModificationInfos, EquipmentAttributeModificationEntity<?>> {
    @Override
    public EquipmentAttributeModificationEntity<?> toEntity(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        return createEntity(equipmentAttributeModificationInfos);
    }

    private <T> EquipmentAttributeModificationEntity<?> createEntity(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        EquipmentAttributeModificationEntity<?> modification;
        var equipmentAttributeValue = equipmentAttributeModificationInfos.getEquipmentAttributeValue();
        if (equipmentAttributeValue == null) {
            modification = new StringEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
        } else {
            switch (equipmentAttributeValue.getClass().getSimpleName()) {
                case "String":
                    modification = new StringEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    break;
                case "Boolean":
                    modification = new BooleanEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    break;
                case "Integer":
                    modification = new IntegerEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    break;
                case "Float":
                    modification = new FloatEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    break;
                case "Double":
                    modification = new DoubleEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    break;
                default:
                    if (equipmentAttributeValue.getClass().isEnum()) {
                        modification = new StringEquipmentAttributeModificationEntity(equipmentAttributeModificationInfos);
                    } else {
                        throw new PowsyblException("Value type invalid : " + equipmentAttributeValue.getClass().getSimpleName());
                    }
            }
        }

        return modification;
    }

    @Override
    public EquipmentAttributeModificationInfos toDto(EquipmentAttributeModificationEntity<?> entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
