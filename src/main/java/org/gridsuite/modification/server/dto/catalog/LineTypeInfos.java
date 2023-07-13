/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto.catalog;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;

import java.util.UUID;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "category"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = AerialLineTypeInfos.class, name = "AERIAL"),
    @JsonSubTypes.Type(value = UndergroundLineTypeInfos.class, name = "UNDERGROUND"),
})
@Schema(description = "Line Type")
public class LineTypeInfos {
    @EqualsAndHashCode.Exclude
    @Schema(description = "id")
    private UUID id;

    @Schema(description = "Type Name")
    private String type;

    @Schema(description = "Voltage")
    private Integer voltage;

    // same for Conductor type (Aerial) or Conductor (Underground)
    @Schema(description = "Conductor type")
    private String conductorType;

    @Schema(description = "Section")
    private Double section;

    @Schema(description = "Linear resistance")
    private Double linearResistance;

    @Schema(description = "Linear reactance")
    private Double linearReactance;

    @Schema(description = "Linear capacity")
    private Double linearCapacity;

    @JsonIgnore
    public LineTypeEntity toEntity() {
        throw new UnsupportedOperationException("TODO");
    }
}
