/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.entities.equipment.modification.ScalingVariationEntity;

import java.util.List;
import java.util.UUID;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Scaling creation")
public class ScalingVariationInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "filters")
    private List<FilterInfos> filters;

    @Schema(description = "variation mode")
    private VariationMode variationMode;

    @Schema(description = "variation value")
    private Double variationValue;

    @Schema(description = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    public ScalingVariationEntity toEntity() {
        return new ScalingVariationEntity(this);
    }
}
