/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import java.time.ZonedDateTime;
import java.util.Set;
import java.util.UUID;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type",
    include = JsonTypeInfo.As.EXISTING_PROPERTY,
    visible = true
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = LoadCreationInfos.class, name = "LOAD_CREATION"),
    @JsonSubTypes.Type(value = LoadModificationInfos.class, name = "LOAD_MODIFICATION"),
    @JsonSubTypes.Type(value = GeneratorCreationInfos.class, name = "GENERATOR_CREATION"),
    @JsonSubTypes.Type(value = GeneratorModificationInfos.class, name = "GENERATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = LineCreationInfos.class, name = "LINE_CREATION"),
    @JsonSubTypes.Type(value = SubstationCreationInfos.class, name = "SUBSTATION_CREATION"),
    @JsonSubTypes.Type(value = VoltageLevelCreationInfos.class, name = "VOLTAGE_LEVEL_CREATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorCreationInfos.class, name = "SHUNT_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreationInfos.class, name = "TWO_WINDINGS_TRANSFORMER_CREATION"),
    @JsonSubTypes.Type(value = EquipmentDeletionInfos.class, name = "EQUIPMENT_DELETION"),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevelInfos.class, name = "LINE_SPLIT_WITH_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevelInfos.class, name = "LINE_ATTACH_TO_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LinesAttachToSplitLinesInfos.class, name = "LINES_ATTACH_TO_SPLIT_LINES"),
    @JsonSubTypes.Type(value = BranchStatusModificationInfos.class, name = "BRANCH_STATUS")
})
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString
@Schema(description = "Modification attributes")
public class ModificationInfos {
    @Schema(description = "Modification id")
    private UUID uuid;

    @Schema(description = "Modification date")
    ZonedDateTime date;

    @Schema(description = "Modification type")
    ModificationType type;

    @Schema(description = "Substations ID")
    @Builder.Default
    private Set<String> substationIds = Set.of();
}
