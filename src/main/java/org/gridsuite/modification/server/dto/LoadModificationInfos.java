/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LoadModification;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.powsybl.iidm.network.LoadType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@JsonInclude(JsonInclude.Include.NON_NULL)
@Schema(description = "Load modification")
@JsonTypeName("LOAD_MODIFICATION")
@ModificationErrorTypeName("MODIFY_LOAD_ERROR")
public class LoadModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Load type modification")
    private AttributeModification<LoadType> loadType;

    @Schema(description = "Active power modification")
    private AttributeModification<Double> p0;

    @Schema(description = "Reactive power modification")
    private AttributeModification<Double> q0;

    @Override
    public LoadModificationEntity toEntity() {
        return new LoadModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new LoadModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Load modification ${equipmentId}")
                .withUntypedValue("equipmentId", getEquipmentId())
                .add();
    }
}
