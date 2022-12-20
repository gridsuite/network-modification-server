/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;

import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "floatEquipmentAttributeModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "float_equipment_attribute_modification_id_fk_constraint"))
public class FloatEquipmentAttributeModificationEntity extends EquipmentAttributeModificationEntity<Float> {
    public FloatEquipmentAttributeModificationEntity(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        super(equipmentAttributeModificationInfos.getEquipmentId(),
            equipmentAttributeModificationInfos.getEquipmentAttributeName(),
            (float) equipmentAttributeModificationInfos.getEquipmentAttributeValue(),
            equipmentAttributeModificationInfos.getEquipmentType());
    }
}
