/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import javax.persistence.*;

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

    public EquipmentDeletionEntity(EquipmentDeletionInfos equipmentDeletionInfos) {
        super(equipmentDeletionInfos);
        assignAttributes(equipmentDeletionInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((EquipmentDeletionInfos) modificationInfos);
    }

    private void assignAttributes(EquipmentDeletionInfos equipmentDeletionInfos) {
        this.equipmentType = equipmentDeletionInfos.getEquipmentType();
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
            .equipmentId(getEquipmentId())
            .equipmentType(getEquipmentType());
    }
}
