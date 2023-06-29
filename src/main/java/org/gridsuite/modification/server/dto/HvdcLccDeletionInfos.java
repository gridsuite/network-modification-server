/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.deletion.ShuntCompensatorSelectionEmbeddable;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author David Braquart<david.braquart at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Hvdc with Lcc deletion")
public class HvdcLccDeletionInfos extends AbstractEquipmentDeletionInfos {

    @Builder
    @Getter
    @EqualsAndHashCode
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
        mcsOnSide1 = mcs1 != null ? mcs1
                .stream()
                .map(s -> new ShuntCompensatorInfos.ShuntCompensatorInfosBuilder()
                        .id(s.getShuntCompensatorId())
                        .connectedToHvdc(s.isConnectedToHvdc()).build())
                .collect(Collectors.toList()) : null;
        mcsOnSide2 = mcs2 != null ? mcs2
                .stream()
                .map(s -> new ShuntCompensatorInfos.ShuntCompensatorInfosBuilder()
                        .id(s.getShuntCompensatorId())
                        .connectedToHvdc(s.isConnectedToHvdc()).build())
                .collect(Collectors.toList()) : null;
    }
}
