/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.Country;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.SubstationModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.SubstationModification;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Substation modification")
@JsonTypeName("SUBSTATION_MODIFICATION")
@ModificationErrorTypeName("MODIFY_SUBSTATION_ERROR")
public class SubstationModificationInfos extends BasicEquipmentModificationInfos {
    @Schema(description = "country modification")
    private AttributeModification<Country> country;

    @Override
    public SubstationModificationEntity toEntity() {
        return new SubstationModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new SubstationModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Substation modification ${equipmentId}")
                .withUntypedValue("equipmentId", this.getEquipmentId())
                .add();
    }
}
