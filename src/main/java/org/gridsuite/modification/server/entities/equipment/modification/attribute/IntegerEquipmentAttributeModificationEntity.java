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

import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "integerEquipmentAttributeModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "integer_equipment_attribute_modification_id_fk_constraint"))
public class IntegerEquipmentAttributeModificationEntity extends EquipmentAttributeModificationEntity<Integer> {
    public IntegerEquipmentAttributeModificationEntity(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        super(equipmentAttributeModificationInfos);
    }
}
