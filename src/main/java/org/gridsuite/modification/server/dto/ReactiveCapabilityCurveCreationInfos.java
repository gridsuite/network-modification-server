/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Generator reactive capability curve point creation")
public class ReactiveCapabilityCurveCreationInfos {
    @Schema(description = "Minimum reactive power ")
    private Double qminP;

    @Schema(description = "Maximum reactive power")
    private Double qmaxP;

    @Schema(description = "Active Power")
    private Double p;
}
