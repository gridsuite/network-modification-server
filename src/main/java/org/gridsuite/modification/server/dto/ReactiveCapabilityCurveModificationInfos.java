/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
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

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Generator reactive capability curve point creation")
public class ReactiveCapabilityCurveModificationInfos {
    @Schema(description = "Minimum reactive power ")
    private Double minQ;

    @Schema(description = "Old minimum reactive power ")
    private Double oldMinQ;

    @Schema(description = "Maximum reactive power")
    private Double maxQ;

    @Schema(description = "Old maximum reactive power")
    private Double oldMaxQ;

    @Schema(description = "Active Power")
    private Double p;

    @Schema(description = "Active Power")
    private Double oldP;
}
