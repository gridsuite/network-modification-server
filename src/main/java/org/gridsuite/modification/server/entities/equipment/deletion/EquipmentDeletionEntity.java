/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "equipmentDeletion")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "equipmentDeletion_id_fk_constraint"))
public class EquipmentDeletionEntity extends EquipmentModificationEntity {
    @Enumerated(EnumType.STRING)
    @Column(name = "equipmentType")
    private IdentifiableType equipmentType;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "additional_equipment_deletion_entity_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "additional_equipment_deletion_entity_id_fk"
            ), nullable = true)
    private AbstractEquipmentDeletionEntity equipmentInfos;

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
        equipmentInfos = equipmentDeletionInfos.getEquipmentInfos() != null ?
            new HvdcLccDeletionEntity(equipmentDeletionInfos.getEquipmentInfos()) : null;
    }

    @Override
    public EquipmentDeletionInfos toModificationInfos() {
        var builder = EquipmentDeletionInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentType(getEquipmentType());
        if (equipmentInfos != null) {
            builder.equipmentInfos(equipmentInfos.toDto());
        }
        return builder.build();
    }
}
