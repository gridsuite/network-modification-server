/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
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

import java.util.List;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "TapChanger attributes")
public class TapChangerCreationInfos {

    @Schema(description = "lowTapPosition")
    private int lowTapPosition;

    @Schema(description = "tapPosition")
    private int tapPosition;

    @Schema(description = "regulating")
    private boolean regulating;

    @Schema(description = "targetDeadband")
    private Double targetDeadband;

    @Schema(description = "Regulating terminal equipment id")
    private String regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private String regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private String regulatingTerminalVlId;

    @Schema(description = "steps")
    private List<TapChangerStepCreationInfos> steps;
}
