/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.server.dto.catalog;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.catalog.TemporaryLimitEntity;

import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Schema(description = "Temporary limit infos")
public class TemporaryLimitInfos {
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "Temporary limit value")
    private Double limitValue;

    @Schema(description = "Temporary limit acceptable duration")
    private Integer acceptableDuration;

    @Schema(description = "Temporary limit name")
    private String name;

    public TemporaryLimitEntity toTemporaryLimitEntity() {
        return TemporaryLimitEntity.builder()
                .limitValue(limitValue)
                .acceptableDuration(acceptableDuration)
                .name(name).build();
    }
}
