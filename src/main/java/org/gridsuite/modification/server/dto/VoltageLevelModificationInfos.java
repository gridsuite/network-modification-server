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
import org.w3c.dom.Attr;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage level modification")
public class VoltageLevelModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "substation id")
    private AttributeModification<String> substationId;

    @Schema(description = "nominal voltage in kV")
    private AttributeModification<Double> nominalVoltage;

    @Schema(description = "low voltage limit in kV")
    private AttributeModification<Double> lowVoltageLimit;

    @Schema(description = "high voltage limit  in kV")
    private AttributeModification<Double> highVoltageLimit;

    @Schema(description = "low short-circuit current limit in kA")
    private AttributeModification<Double> lowShortCircuitCurrentLimit;

    @Schema(description = "high short-circuit current limit in kA")
    private AttributeModification<Double> highShortCircuitCurrentLimit;
}
