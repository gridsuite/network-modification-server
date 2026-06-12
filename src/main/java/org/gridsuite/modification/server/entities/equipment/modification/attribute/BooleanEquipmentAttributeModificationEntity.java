/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.EquipmentAttributeModificationModel;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "booleanEquipmentAttributeModification",
        indexes = @Index(name = "boolean_equipment_attribute_modif_vl_topology_modif_id_idx", columnList = "voltage_level_topology_modification_id"))
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "boolean_equipment_attribute_modification_id_fk_constraint"))
public class BooleanEquipmentAttributeModificationEntity extends EquipmentAttributeModificationEntity<Boolean> {

    public BooleanEquipmentAttributeModificationEntity(ModificationInfos equipmentAttributeModificationInfos) {
        super(equipmentAttributeModificationInfos);
    }

    public BooleanEquipmentAttributeModificationEntity(EquipmentAttributeModificationModel equipmentAttributeModificationInfos) {
        super(equipmentAttributeModificationInfos);
    }
}
