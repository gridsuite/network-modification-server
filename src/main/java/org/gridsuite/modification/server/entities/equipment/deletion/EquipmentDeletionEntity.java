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
import org.gridsuite.modification.server.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import javax.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

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

    @ElementCollection
    @CollectionTable(name = "shuntCompensatorsSide1")
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide1;

    @ElementCollection
    @CollectionTable(name = "shuntCompensatorsSide2")
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide2;

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
        if (equipmentDeletionInfos.getSpecificData() instanceof HvdcLccDeletionInfos) {
            HvdcLccDeletionInfos specificInfos = (HvdcLccDeletionInfos) equipmentDeletionInfos.getSpecificData();
            this.shuntCompensatorsSide1 = toEmbeddableShuntCompensators(specificInfos.getMcsOnSide1());
            this.shuntCompensatorsSide2 = toEmbeddableShuntCompensators(specificInfos.getMcsOnSide2());
        }
    }

    @Override
    public EquipmentDeletionInfos toModificationInfos() {
        var builder = EquipmentDeletionInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .equipmentId(getEquipmentId())
                .equipmentType(getEquipmentType());
        if (shuntCompensatorsSide1 != null && !shuntCompensatorsSide1.isEmpty() ||
                shuntCompensatorsSide2 != null && !shuntCompensatorsSide2.isEmpty()) {
            builder.specificData(new HvdcLccDeletionInfos(shuntCompensatorsSide1, shuntCompensatorsSide2));
        }
        return builder.build();
    }

    private static List<ShuntCompensatorSelectionEmbeddable> toEmbeddableShuntCompensators(List<HvdcLccDeletionInfos.ShuntCompensatorInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
                .map(s -> new ShuntCompensatorSelectionEmbeddable(s.getId(), s.isConnectedToHvdc()))
                .collect(Collectors.toList());
    }
}
