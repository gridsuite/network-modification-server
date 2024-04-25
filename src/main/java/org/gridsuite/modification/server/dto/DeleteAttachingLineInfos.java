/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteAttachingLineEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.DeleteAttachingLine;

import java.util.HashMap;
import java.util.Map;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Delete attaching line infos")
@JsonTypeName("DELETE_ATTACHING_LINE")
@ModificationErrorTypeName("DELETE_ATTACHING_LINE_ERROR")
public class DeleteAttachingLineInfos extends ModificationInfos {

    @Schema(description = "line 1 id")
    private String lineToAttachTo1Id;

    @Schema(description = "line 2 id")
    private String lineToAttachTo2Id;

    @Schema(description = "attachment line id")
    private String attachedLineId;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine1Id;

    @Schema(description = "replacing line 1 name")
    private String replacingLine1Name;

    @Override
    public DeleteAttachingLineEntity toEntity() {
        return new DeleteAttachingLineEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new DeleteAttachingLine(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate(getType().name(), "Delete attaching line").add();
    }

    @Override
    public Map<String, String> getMapMessageValues() {
        Map<String, String> mapMessageValues = new HashMap<>();
        mapMessageValues.put("attachedLineId", getAttachedLineId());
        mapMessageValues.put("lineToAttachTo1Id", getLineToAttachTo1Id());
        mapMessageValues.put("lineToAttachTo2Id", getLineToAttachTo2Id());
        return mapMessageValues;
    }
}
