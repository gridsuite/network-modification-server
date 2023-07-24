/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "RatioTapChanger attributes")
public class RatioTapChangerModificationInfos extends TapChangerModificationInfos {

    @Schema(description = "enabled")
    private AttributeModification<Boolean> enabled;

    @Schema(description = "loadTapChangingCapabilities")
    private AttributeModification<Boolean> loadTapChangingCapabilities;

    @Schema(description = "targetV")
    private AttributeModification<Double> targetV;
}