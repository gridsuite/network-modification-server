/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Substation free properties")
public class SubstationFreePropertyInfos {
    @Schema(description = "property name")
    private String name;

    @Schema(description = "property value")
    private String value;

    @Builder.Default
    @Schema(description = "marked as deleted")
    private boolean deletionMark = false;

    @Builder.Default
    @Schema(description = "property added in current modification")
    private boolean added = false;
}
