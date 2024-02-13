/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import lombok.*;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.TabularModification;
import org.springframework.lang.NonNull;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.experimental.SuperBuilder;

import java.time.ZonedDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular modification")
@JsonTypeName("TABULAR_MODIFICATION")
@ModificationErrorTypeName("TABULAR_MODIFICATION_ERROR")
public class TabularModificationInfos extends ModificationInfos {

    @Schema(description = "Modification type")
    @NonNull
    private ModificationType modificationType;

    @Schema(description = "modifications")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<ModificationInfos> modifications;

    public TabularModificationInfos(UUID uuid, ZonedDateTime date, Boolean stashed, @NonNull ModificationType modificationType, List<ModificationInfos> modifications) {
        super(uuid, date, stashed);
        this.modificationType = modificationType;
        this.modifications = modifications;
    }

    @Override
    public ModificationEntity toEntity() {
        return new TabularModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new TabularModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.TABULAR_MODIFICATION.name(), "Tabular modification");
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("tabularModificationType", getModificationType().name());
        return mapMessageValues;
    }
}
