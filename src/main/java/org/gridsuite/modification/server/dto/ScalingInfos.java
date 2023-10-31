/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.VariationType;

import java.util.List;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Scaling infos")
public class ScalingInfos extends ModificationInfos {
    @Schema(description = "scaling variations")
    private List<ScalingVariationInfos> variations;

    @Schema(description = "variation type")
    private VariationType variationType;
}
