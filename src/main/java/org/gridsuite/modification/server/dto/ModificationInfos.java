/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;

import java.time.ZonedDateTime;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@JsonTypeInfo(
    use = JsonTypeInfo.Id.NAME,
    property = "type"
)
@JsonSubTypes({
    @JsonSubTypes.Type(value = GroovyScriptInfos.class, name = "GROOVY_SCRIPT"),
    @JsonSubTypes.Type(value = BatteryCreationInfos.class, name = "BATTERY_CREATION"),
    @JsonSubTypes.Type(value = BatteryModificationInfos.class, name = "BATTERY_MODIFICATION"),
    @JsonSubTypes.Type(value = LoadCreationInfos.class, name = "LOAD_CREATION"),
    @JsonSubTypes.Type(value = LoadModificationInfos.class, name = "LOAD_MODIFICATION"),
    @JsonSubTypes.Type(value = GeneratorCreationInfos.class, name = "GENERATOR_CREATION"),
    @JsonSubTypes.Type(value = GeneratorModificationInfos.class, name = "GENERATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = LineCreationInfos.class, name = "LINE_CREATION"),
    @JsonSubTypes.Type(value = LineModificationInfos.class, name = "LINE_MODIFICATION"),
    @JsonSubTypes.Type(value = SubstationCreationInfos.class, name = "SUBSTATION_CREATION"),
    @JsonSubTypes.Type(value = SubstationModificationInfos.class, name = "SUBSTATION_MODIFICATION"),
    @JsonSubTypes.Type(value = VoltageLevelCreationInfos.class, name = "VOLTAGE_LEVEL_CREATION"),
    @JsonSubTypes.Type(value = VoltageLevelModificationInfos.class, name = "VOLTAGE_LEVEL_MODIFICATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorCreationInfos.class, name = "SHUNT_COMPENSATOR_CREATION"),
    @JsonSubTypes.Type(value = ShuntCompensatorModificationInfos.class, name = "SHUNT_COMPENSATOR_MODIFICATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreationInfos.class, name = "TWO_WINDINGS_TRANSFORMER_CREATION"),
    @JsonSubTypes.Type(value = TwoWindingsTransformerModificationInfos.class, name = "TWO_WINDINGS_TRANSFORMER_MODIFICATION"),
    @JsonSubTypes.Type(value = EquipmentDeletionInfos.class, name = "EQUIPMENT_DELETION"),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevelInfos.class, name = "LINE_SPLIT_WITH_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevelInfos.class, name = "LINE_ATTACH_TO_VOLTAGE_LEVEL"),
    @JsonSubTypes.Type(value = LinesAttachToSplitLinesInfos.class, name = "LINES_ATTACH_TO_SPLIT_LINES"),
    @JsonSubTypes.Type(value = BranchStatusModificationInfos.class, name = "BRANCH_STATUS_MODIFICATION"),
    @JsonSubTypes.Type(value = EquipmentAttributeModificationInfos.class, name = "EQUIPMENT_ATTRIBUTE_MODIFICATION"),
    @JsonSubTypes.Type(value = GeneratorScalingInfos.class, name = "GENERATOR_SCALING"),
    @JsonSubTypes.Type(value = LoadScalingInfos.class, name = "LOAD_SCALING"),
    @JsonSubTypes.Type(value = DeleteVoltageLevelOnLineInfos.class, name = "DELETE_VOLTAGE_LEVEL_ON_LINE"),
    @JsonSubTypes.Type(value = DeleteAttachingLineInfos.class, name = "DELETE_ATTACHING_LINE"),
    @JsonSubTypes.Type(value = GenerationDispatchInfos.class, name = "GENERATION_DISPATCH")
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
    private ZonedDateTime date;

    @JsonIgnore
    public ModificationEntity toEntity() {
        throw new UnsupportedOperationException("TODO");
    }

    @JsonIgnore
    public AbstractModification toModification() {
        throw new UnsupportedOperationException("TODO");
    }

    @JsonIgnore
    public Reporter createSubReporter(ReporterModel reporter) {
        throw new UnsupportedOperationException("TODO");
    }

    @JsonIgnore
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.valueOf(this.getClass().getAnnotation(ModificationErrorTypeName.class).value());
    }

    @JsonIgnore
    public ModificationType getType() {
        return ModificationType.valueOf(this.getClass().getAnnotation(JsonTypeName.class).value());
    }

    @JsonIgnore
    public void check() {
        // To check input DTO before hypothesis creation. Nothing to check here
    }
}
