/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.entities.equipment.modification.SubstationModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.SubstationModification;

import java.util.List;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Substation modification")
public class SubstationModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "country modification")
    private AttributeModification<Country> substationCountry;

    @Schema(description = "free properties")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<SubstationFreePropertyInfos> properties;

    @Override
    public SubstationModificationEntity toEntity() {
        return new SubstationModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new SubstationModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFY_SUBSTATION_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.SUBSTATION_MODIFICATION.name(), "Substation modification ${substationId}", "substationId", this.getEquipmentId());
    }
}
