/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "float_equipment_attribute_modification_id_fk_constraint"))
public class FloatEquipmentAttributeModificationEntity extends EquipmentAttributeModificationEntity<Float> {
    public FloatEquipmentAttributeModificationEntity(String equipmentId, String attributeName, Float attributeValue, IdentifiableType equipmentType) {
        super(equipmentId, attributeName, attributeValue, equipmentType);
    }
}
