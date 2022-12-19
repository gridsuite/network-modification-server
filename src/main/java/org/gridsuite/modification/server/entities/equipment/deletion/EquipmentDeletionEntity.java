/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import static org.gridsuite.modification.server.ModificationType.EQUIPMENT_DELETION;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "equipmentDeletion")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "equipmentDeletion_id_fk_constraint"))
public class EquipmentDeletionEntity extends EquipmentModificationEntity {
    @Column(name = "equipmentType")
    private String equipmentType;

    //TODO : remove
    public EquipmentDeletionEntity(String equipmentId, String equipmentType) {
        super(equipmentId, EQUIPMENT_DELETION);
        this.equipmentType = equipmentType;
    }

    @Override
    public EquipmentDeletionInfos toModificationInfos() {
        return toEquipmentDeletionInfosBuilder().build();
    }

    private EquipmentDeletionInfos.EquipmentDeletionInfosBuilder<?, ?> toEquipmentDeletionInfosBuilder() {
        return EquipmentDeletionInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentType(getEquipmentType());
    }
}
