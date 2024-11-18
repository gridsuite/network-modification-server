/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerEquipmentAttributeModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringEquipmentAttributeModificationEntity;

import com.powsybl.commons.PowsyblException;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
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
        // TODO this method will replace the toModificationInfos method in entity classes
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
