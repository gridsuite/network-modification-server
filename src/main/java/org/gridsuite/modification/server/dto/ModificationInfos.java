/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.Modification;

import java.time.ZonedDateTime;
import java.util.Set;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
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

    public ModificationEntity toEntity() {
        return null;
    }

    public Modification toModification() {
        return null;
    }

    public Reporter createSubReporter(ReporterModel reporter) {
        return null;
    }

    public NetworkModificationException.Type getErrorType() {
        return null;
    }
}
