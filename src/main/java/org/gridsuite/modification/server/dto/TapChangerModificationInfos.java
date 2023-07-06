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
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "TapChanger attributes")
public class TapChangerModificationInfos {

    @Schema(description = "enabled")
    private AttributeModification<Boolean> enabled;

    @Schema(description = "lowTapPosition")
    private AttributeModification<Integer> lowTapPosition;

    @Schema(description = "tapPosition")
    private AttributeModification<Integer> tapPosition;

    @Schema(description = "targetDeadband")
    private AttributeModification<Double> targetDeadband;

    @Schema(description = "Regulating terminal equipment id")
    private AttributeModification<String> regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private AttributeModification<String> regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private AttributeModification<String> regulatingTerminalVlId;

    @Schema(description = "steps")
    private List<TapChangerStepCreationInfos> steps;
}
