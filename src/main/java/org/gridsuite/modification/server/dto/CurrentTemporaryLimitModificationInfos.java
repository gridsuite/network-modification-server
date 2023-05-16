/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@JsonInclude(JsonInclude.Include.NON_NULL)
public class CurrentTemporaryLimitModificationInfos {

    @Schema(description = "name")
    private String name;

    @Schema(description = "value")
    private Double value;

    @Schema(description = "acceptable duration")
    private Integer acceptableDuration;

    @Schema(description = "modification type")
    private TemporaryLimitModificationType modificationType;
}
