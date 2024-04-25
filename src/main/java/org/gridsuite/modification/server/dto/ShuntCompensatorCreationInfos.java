/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
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
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Shunt compensator creation")
@JsonTypeName("SHUNT_COMPENSATOR_CREATION")
@ModificationErrorTypeName("CREATE_SHUNT_COMPENSATOR_ERROR")
public class ShuntCompensatorCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Maximum number of sections")
    private Integer maximumSectionCount;

    @Schema(description = "Section count")
    private Integer sectionCount;

    @Schema(description = "Maximal susceptance available")
    private Double maxSusceptance;

    @Schema(description = "Qmax available at nominal voltage")
    private Double maxQAtNominalV;

    @Schema(description = "Shunt Compensator Type")
    private ShuntCompensatorType shuntCompensatorType;

    @Override
    public ShuntCompensatorCreationEntity toEntity() {
        return new ShuntCompensatorCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new ShuntCompensatorCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Shunt compensator creation ${shuntCompensatorId}")
                .withUntypedValue("shuntCompensatorId", this.getEquipmentId())
                .add();
    }
}
