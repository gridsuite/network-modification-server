/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.MassiveEquipmentsModificationsEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.MassiveEquipmentsModifications;

import java.util.List;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@Schema(description = "Massive equipments modifications infos")
@JsonTypeName("MASSIVE_EQUIPMENTS_MODIFICATIONS")
@ModificationErrorTypeName("MASSIVE_EQUIPMENTS_MODIFICATIONS_ERROR")
public class MassiveEquipmentsModificationsInfos extends ModificationInfos {
    @Schema(description = "equipments modifications")
    private List<ModificationInfos> modifications;

    @JsonIgnore
    public ModificationEntity toEntity() {
        return new MassiveEquipmentsModificationsEntity(this);
    }

    @JsonIgnore
    public AbstractModification toModification() {
        return new MassiveEquipmentsModifications(this);
    }

    @JsonIgnore
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MASSIVE_EQUIPMENTS_MODIFICATIONS_ERROR;
    }

    @JsonIgnore
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.MASSIVE_EQUIPMENTS_MODIFICATIONS.name(), "Massive equipments modifications");
    }
}
