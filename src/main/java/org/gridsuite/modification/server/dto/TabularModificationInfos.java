/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import lombok.*;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.TabularModification;
import org.springframework.lang.NonNull;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Data
@Schema(description = "Tabular modification")
@JsonTypeName("TABULAR_MODIFICATION")
public class TabularModificationInfos extends ModificationInfos {
    @Schema(description = "Modification type")
    @NonNull
    private String modificationType;

    @Schema(description = "modifications")
    private List<ModificationInfos> modifications;

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
}
