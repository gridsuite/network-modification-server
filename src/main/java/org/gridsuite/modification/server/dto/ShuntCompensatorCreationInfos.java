/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.ShuntCompensatorCreation;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Shunt compensator creation")
@JsonTypeName("SHUNT_COMPENSATOR_CREATION")
@ModificationErrorTypeName("CREATE_SHUNT_COMPENSATOR_ERROR")
public class ShuntCompensatorCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Maximum number of sections")
    private Integer maximumNumberOfSections;

    @Schema(description = "Current number of sections")
    private Integer currentNumberOfSections;

    @Schema(description = "Susceptance per section")
    private Double susceptancePerSection;

    @JsonProperty("qAtNominalV")
    @Schema(description = "Q at Nominal Voltage")
    private Double qAtNominalV;

    @Schema(description = "Shunt Compensator Type")
    private ShuntCompensatorType shuntCompensatorType;

    @Schema(description = "Identical sections")
    private Boolean isIdenticalSection;

    @Schema(description = "Connection Name")
    private String connectionName;

    @Schema(description = "Connection Direction")
    private ConnectablePosition.Direction connectionDirection;

    @Schema(description = "Connection Position")
    private Integer connectionPosition;

    @Override
    public ShuntCompensatorCreationEntity toEntity() {
        return new ShuntCompensatorCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new ShuntCompensatorCreation(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Creation of shunt compensator " + getEquipmentId());
    }
}
