/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import org.gridsuite.modification.server.entities.equipment.deletion.AbstractEquipmentDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.HvdcLccDeletionEntity;
import org.gridsuite.modification.server.entities.equipment.deletion.ShuntCompensatorSelectionEmbeddable;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author David Braquart<david.braquart at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Hvdc with Lcc deletion")
public class HvdcLccDeletionInfos extends AbstractEquipmentDeletionInfos {

    @Builder
    @Getter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ShuntCompensatorInfos {
        private String id;
        private boolean connectedToHvdc;
    }

    @Schema(description = "LCC HVDC converter station Shunt Compensator side 1")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorInfos> mcsOnSide1;

    @Schema(description = "LCC HVDC converter station Shunt Compensator side 2")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ShuntCompensatorInfos> mcsOnSide2;

    public HvdcLccDeletionInfos(List<ShuntCompensatorSelectionEmbeddable> mcs1, List<ShuntCompensatorSelectionEmbeddable> mcs2) {
        mcsOnSide1 = toShuntCompensators(mcs1);
        mcsOnSide2 = toShuntCompensators(mcs2);
    }

    @Override
    public AbstractEquipmentDeletionEntity toEntity() {
        return mcsOnSide1 != null && !mcsOnSide1.isEmpty() || mcsOnSide2 != null && !mcsOnSide2.isEmpty() ?
            new HvdcLccDeletionEntity(toEmbeddableShuntCompensators(mcsOnSide1), toEmbeddableShuntCompensators(mcsOnSide2))
            : null;
    }

    private List<ShuntCompensatorSelectionEmbeddable> toEmbeddableShuntCompensators(List<HvdcLccDeletionInfos.ShuntCompensatorInfos> shuntCompensators) {
        return shuntCompensators == null ? null : shuntCompensators.stream()
                .map(s -> new ShuntCompensatorSelectionEmbeddable(s.getId(), s.isConnectedToHvdc()))
                .collect(Collectors.toList());
    }

    private List<ShuntCompensatorInfos> toShuntCompensators(List<ShuntCompensatorSelectionEmbeddable> shuntCompensators) {
        return shuntCompensators != null ? shuntCompensators.stream()
            .map(s -> new ShuntCompensatorInfos.ShuntCompensatorInfosBuilder()
                .id(s.getShuntCompensatorId())
                .connectedToHvdc(s.isConnectedToHvdc()).build())
            .collect(Collectors.toList()) : null;
    }
}
