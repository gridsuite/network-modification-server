/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import jakarta.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.dto.AbstractEquipmentDeletionInfos;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos.ShuntCompensatorInfos;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Entity
@Getter
@Table(name = "hvdcLccDeletion")
public class HvdcLccDeletionEntity extends AbstractEquipmentDeletionEntity {
    @ElementCollection
    @CollectionTable(name = "HvdcLccDeletionShuntCompensatorsSide1",
        indexes = {@Index(name = "HvdcLccDeletionEntity_shuntCompensatorsSide1_idx1", columnList = "hvdc_lcc_deletion_entity_id")},
        foreignKey = @ForeignKey(name = "HvdcLccDeletionEntity_shuntCompensatorsSide1_fk1"))
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide1;

    @ElementCollection
    @CollectionTable(name = "HvdcLccDeletionShuntCompensatorsSide2",
        indexes = {@Index(name = "HvdcLccDeletionEntity_shuntCompensatorsSide2_idx1", columnList = "hvdc_lcc_deletion_entity_id")},
        foreignKey = @ForeignKey(name = "HvdcLccDeletionEntity_shuntCompensatorsSide2_fk1"))
    private List<ShuntCompensatorSelectionEmbeddable> shuntCompensatorsSide2;

    @Override
    public HvdcLccDeletionInfos toDto() {
        var shuntCompensatorsSide1 = this.getShuntCompensatorsSide1();
        var shuntCompensatorsSide2 = this.getShuntCompensatorsSide2();
        if (CollectionUtils.isNotEmpty(shuntCompensatorsSide1) || CollectionUtils.isNotEmpty(shuntCompensatorsSide2)) {
            var hvdcLccDeletionInfos = new HvdcLccDeletionInfos();
            hvdcLccDeletionInfos.setMcsOnSide1(toShuntCompensators(shuntCompensatorsSide1));
            hvdcLccDeletionInfos.setMcsOnSide2(toShuntCompensators(shuntCompensatorsSide2));
            return hvdcLccDeletionInfos;
        }
        return null;
    }

    private List<ShuntCompensatorInfos> toShuntCompensators(List<ShuntCompensatorSelectionEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators.stream()
            .map(s -> ShuntCompensatorInfos.builder()
                .id(s.getShuntCompensatorId())
                .connectedToHvdc(s.isConnectedToHvdc()).build())
            .collect(Collectors.toList()) : null;
    }

    public HvdcLccDeletionEntity(AbstractEquipmentDeletionInfos equipmentDeletionInfos) {
        var dto = (HvdcLccDeletionInfos) equipmentDeletionInfos;
        if (dto.getMcsOnSide1() != null && !dto.getMcsOnSide1().isEmpty() || dto.getMcsOnSide2() != null && !dto.getMcsOnSide2().isEmpty()) {
            new HvdcLccDeletionEntity(toEmbeddableShuntCompensators(dto.getMcsOnSide1()), toEmbeddableShuntCompensators(dto.getMcsOnSide2()));
        }
    }

    private List<ShuntCompensatorSelectionEmbeddable> toEmbeddableShuntCompensators(List<HvdcLccDeletionInfos.ShuntCompensatorInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
                .map(s -> new ShuntCompensatorSelectionEmbeddable(s.getId(), s.isConnectedToHvdc()))
                .collect(Collectors.toList());
    }
}
