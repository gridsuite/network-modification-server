/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.catalog;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import java.util.UUID;

import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@EqualsAndHashCode
@Schema(description = "Line Type")
public class LineType {

    @EqualsAndHashCode.Exclude
    @Schema(description = "id")
    UUID id;

    @Schema(description = "Category (AERIAL or UNDERGROUND)")
    private LineTypeCategory category;

    @Schema(description = "Type Name")
    private String type;

    @Schema(description = "Voltage")
    private Integer voltage;

    @Schema(description = "Conductor type")
    private String conductorType;

    @Schema(description = "Section")
    private Double section;

    @Schema(description = "Number of conductors")
    private Integer conductorsNumber;

    @Schema(description = "Number of circuits")
    private Integer circuitsNumber;

    @Schema(description = "Number of ground wires")
    private Integer groundWiresNumber;

    @Schema(description = "Linear resistance")
    private Double linearResistance;

    @Schema(description = "Linear reactance")
    private Double linearReactance;

    @Schema(description = "Linear capacity")
    private Double linearCapacity;

    public LineTypeEntity toEntity() {
        return new LineTypeEntity(this);
    }
}
