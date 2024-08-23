/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Schema(description = "Static var compensator automaton creation")
public class StandByAutomatonCreationInfos {
    @Schema(description = "Minimum reactive power ")
    private boolean standby;

    @Schema(description = "Fix part of the susceptance")
    private double b0;

    @Schema(description = "Low voltage set point ")
    private double lowVoltageSetpoint;

    @Schema(description = "High voltage set point")
    private double highVoltageSetpoint;

    @Schema(description = "Low voltage threshold")
    private double lowVoltageThreshold;

    @Schema(description = "High voltage threshold")
    private double highVoltageThreshold;
}
