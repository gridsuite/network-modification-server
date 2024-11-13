/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos;
import org.gridsuite.modification.dto.HvdcLccDeletionInfos.ShuntCompensatorInfos;
import org.gridsuite.modification.server.entities.equipment.deletion.HvdcLccDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.ShuntCompensatorSelectionEmbeddable;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class HvdcLccDeletionMapper implements EntityMapper<HvdcLccDeletionInfos, HvdcLccDeletionEntity> {
    @Override
    public HvdcLccDeletionEntity toEntity(HvdcLccDeletionInfos dto) {
        return dto.getMcsOnSide1() != null && !dto.getMcsOnSide1().isEmpty() || dto.getMcsOnSide2() != null && !dto.getMcsOnSide2().isEmpty() ?
            new HvdcLccDeletionEntity(toEmbeddableShuntCompensators(dto.getMcsOnSide1()), toEmbeddableShuntCompensators(dto.getMcsOnSide2()))
            : null;
    }

    @Override
    public HvdcLccDeletionInfos toDto(HvdcLccDeletionEntity entity) {
        var shuntCompensatorsSide1 = entity.getShuntCompensatorsSide1();
        var shuntCompensatorsSide2 = entity.getShuntCompensatorsSide2();
        if (CollectionUtils.isNotEmpty(shuntCompensatorsSide1) || CollectionUtils.isNotEmpty(shuntCompensatorsSide2)) {
            var hvdcLccDeletionInfos = new HvdcLccDeletionInfos();
            hvdcLccDeletionInfos.setMcsOnSide1(toShuntCompensators(shuntCompensatorsSide1));
            hvdcLccDeletionInfos.setMcsOnSide2(toShuntCompensators(shuntCompensatorsSide2));
            return hvdcLccDeletionInfos;
        }
        return null;
    }

    private List<ShuntCompensatorSelectionEmbeddable> toEmbeddableShuntCompensators(List<HvdcLccDeletionInfos.ShuntCompensatorInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
                .map(s -> new ShuntCompensatorSelectionEmbeddable(s.getId(), s.isConnectedToHvdc()))
                .collect(Collectors.toList());
    }

    private List<ShuntCompensatorInfos> toShuntCompensators(List<ShuntCompensatorSelectionEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators.stream()
            .map(s -> ShuntCompensatorInfos.builder()
                .id(s.getShuntCompensatorId())
                .connectedToHvdc(s.isConnectedToHvdc()).build())
            .collect(Collectors.toList()) : null;
    }
}
