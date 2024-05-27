/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.entities.equipment.modification.LoadScalingEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LoadScaling;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Load scaling creation")
@JsonTypeName("LOAD_SCALING")
@ModificationErrorTypeName("LOAD_SCALING_ERROR")
public class LoadScalingInfos extends ScalingInfos {

    @Override
    public LoadScalingEntity toEntity() {
        return new LoadScalingEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new LoadScaling(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Load Scaling")
                .add();
    }
}
