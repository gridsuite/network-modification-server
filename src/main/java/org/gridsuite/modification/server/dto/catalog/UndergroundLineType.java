/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.catalog;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.server.entities.catalog.UndergroundLineTypeEntity;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@EqualsAndHashCode(callSuper = true)
@Schema(description = "Underground Line Type")
public class UndergroundLineType extends LineType {

    @Schema(description = "Insulator")
    private String insulator;

    @Schema(description = "Screen")
    private String screen;

    @Override
    public UndergroundLineTypeEntity toEntity() {
        return new UndergroundLineTypeEntity(this);
    }
}
