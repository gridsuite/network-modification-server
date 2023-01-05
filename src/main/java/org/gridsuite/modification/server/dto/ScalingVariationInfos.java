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
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.entities.equipment.modification.ScalingVariationEntity;

import java.util.List;
import java.util.UUID;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Scaling creation")
public class ScalingVariationInfos {
    @Schema(description = "id")
    UUID id;

    @Schema(description = "filters")
    List<FilterInfos> filters;

    @Schema(description = "variation mode")
    VariationMode variationMode;

    @Schema(description = "variation value")
    Double variationValue;

    public ScalingVariationEntity toEntity() {
        return new ScalingVariationEntity(this);
    }
}
