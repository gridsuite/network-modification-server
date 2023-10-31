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

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Branch creation")
public class BranchModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "Series resistance")
    private AttributeModification<Double> seriesResistance;

    @Schema(description = "Series reactance")
    private AttributeModification<Double> seriesReactance;

    @Schema(description = "Current limits Side 1")
    private CurrentLimitsModificationInfos currentLimits1;

    @Schema(description = "Current limits Side 2")
    private CurrentLimitsModificationInfos currentLimits2;
}
