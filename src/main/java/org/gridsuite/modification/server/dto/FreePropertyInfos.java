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
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Free properties")
public class FreePropertyInfos {
    @Schema(description = "property name")
    private String name;

    @Schema(description = "property value")
    private String value;

    @Schema(description = "marked as deleted")
    private boolean deletionMark = false;

    @Schema(description = "property added in current modification")
    private boolean added = false;

    @Schema(description = "previous value")
    private String previousValue;

    public FreePropertyEntity toEntity() {
        return FreePropertyEntity.builder()
            .name(getName())
            .value(getValue())
            .deletionMark(isDeletionMark())
            .added(isAdded())
            .previousValue(getPreviousValue())
            .build();
    }
}
