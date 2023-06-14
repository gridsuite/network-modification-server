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
import org.gridsuite.modification.server.dto.ShuntCompensatorSelectionInfos;
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

    @Column(name = "hvdcWithLCC", columnDefinition = "boolean default false")
    private boolean hvdcWithLCC;

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
        this.hvdcWithLCC = equipmentDeletionInfos.isHvdcWithLCC();
        this.shuntCompensatorsSide1 = toEmbeddableShuntCompensators(equipmentDeletionInfos.getMcsOnSide1());
        this.shuntCompensatorsSide2 = toEmbeddableShuntCompensators(equipmentDeletionInfos.getMcsOnSide2());
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
            .equipmentType(getEquipmentType())
            .hvdcWithLCC(isHvdcWithLCC())
            .mcsOnSide1(toShuntCompensators(getShuntCompensatorsSide1()))
            .mcsOnSide2(toShuntCompensators(getShuntCompensatorsSide2()));
    }

    private List<ShuntCompensatorSelectionInfos> toShuntCompensators(List<ShuntCompensatorSelectionEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators
                .stream()
                .map(s -> new ShuntCompensatorSelectionInfos(s.getShuntCompensatorId(), s.isSelected()))
                .collect(Collectors.toList()) : null;
    }

    private static List<ShuntCompensatorSelectionEmbeddable> toEmbeddableShuntCompensators(List<ShuntCompensatorSelectionInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
                .map(s -> new ShuntCompensatorSelectionEmbeddable(s.getId(), s.isSelected()))
                .collect(Collectors.toList());
    }
}
