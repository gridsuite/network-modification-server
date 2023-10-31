/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.catalog;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.catalog.AerialLineTypeEntity;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Schema(description = "Aerial Line Type")
public class AerialLineTypeInfos extends LineTypeInfos {
    @Schema(description = "Number of conductors")
    private Integer conductorsNumber;

    @Schema(description = "Number of circuits")
    private Integer circuitsNumber;

    @Schema(description = "Number of ground wires")
    private Integer groundWiresNumber;

    @Override
    public AerialLineTypeEntity toEntity() {
        return new AerialLineTypeEntity(this);
    }
}
