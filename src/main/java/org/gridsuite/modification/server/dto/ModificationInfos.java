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
    @JsonSubTypes.Type(value = GroovyScriptInfos.class),
    @JsonSubTypes.Type(value = BatteryCreationInfos.class),
    @JsonSubTypes.Type(value = BatteryModificationInfos.class),
    @JsonSubTypes.Type(value = LoadCreationInfos.class),
    @JsonSubTypes.Type(value = LoadModificationInfos.class),
    @JsonSubTypes.Type(value = GeneratorCreationInfos.class),
    @JsonSubTypes.Type(value = GeneratorModificationInfos.class),
    @JsonSubTypes.Type(value = LineCreationInfos.class),
    @JsonSubTypes.Type(value = LineModificationInfos.class),
    @JsonSubTypes.Type(value = SubstationCreationInfos.class),
    @JsonSubTypes.Type(value = SubstationModificationInfos.class),
    @JsonSubTypes.Type(value = VoltageLevelCreationInfos.class),
    @JsonSubTypes.Type(value = VoltageLevelModificationInfos.class),
    @JsonSubTypes.Type(value = ShuntCompensatorCreationInfos.class),
    @JsonSubTypes.Type(value = ShuntCompensatorModificationInfos.class),
    @JsonSubTypes.Type(value = TwoWindingsTransformerCreationInfos.class),
    @JsonSubTypes.Type(value = TwoWindingsTransformerModificationInfos.class),
    @JsonSubTypes.Type(value = EquipmentDeletionInfos.class),
    @JsonSubTypes.Type(value = LineSplitWithVoltageLevelInfos.class),
    @JsonSubTypes.Type(value = LineAttachToVoltageLevelInfos.class),
    @JsonSubTypes.Type(value = LinesAttachToSplitLinesInfos.class),
    @JsonSubTypes.Type(value = BranchStatusModificationInfos.class),
    @JsonSubTypes.Type(value = EquipmentAttributeModificationInfos.class),
    @JsonSubTypes.Type(value = GeneratorScalingInfos.class),
    @JsonSubTypes.Type(value = LoadScalingInfos.class),
    @JsonSubTypes.Type(value = DeleteVoltageLevelOnLineInfos.class),
    @JsonSubTypes.Type(value = DeleteAttachingLineInfos.class),
    @JsonSubTypes.Type(value = GenerationDispatchInfos.class),
    @JsonSubTypes.Type(value = VoltageInitModificationInfos.class),
    @JsonSubTypes.Type(value = VscCreationInfos.class),
    @JsonSubTypes.Type(value = ConverterStationCreationInfos.class),
    @JsonSubTypes.Type(value = TabularModificationInfos.class),
    @JsonSubTypes.Type(value = ByFormulaModificationInfos.class)
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

    @Schema(description = "Modification flag")
    private Boolean stashed;

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
    public final NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.valueOf(this.getClass().getAnnotation(ModificationErrorTypeName.class).value());
    }

    @JsonIgnore
    public final ModificationType getType() {
        return ModificationType.valueOf(this.getClass().getAnnotation(JsonTypeName.class).value());
    }

    @JsonIgnore
    public void check() {
        // To check input DTO before hypothesis creation. Nothing to check here
    }
}
