/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Country;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.creation.SubstationCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.SubstationCreation;

import java.util.Map;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Substation creation")
public class SubstationCreationInfos extends EquipmentCreationInfos {

    @Schema(description = "Substation country")
    private Country substationCountry;

    @Schema(description = "free properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private Map<String, String> properties;

    @Override
    public SubstationCreationEntity toEntity() {
        return new SubstationCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new SubstationCreation(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_SUBSTATION_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.SUBSTATION_CREATION.name(), "Substation creation ${substationId}", "substationId", getEquipmentId());
    }
}
