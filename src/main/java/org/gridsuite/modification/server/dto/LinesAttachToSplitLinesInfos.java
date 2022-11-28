/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.LinesAttachToSplitLinesEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LinesAttachToSplitLines;


/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line attach to split line")
public class LinesAttachToSplitLinesInfos extends ModificationInfos {

    @Schema(description = "line 1 id")
    private String lineToAttachTo1Id;

    @Schema(description = "line 2 id")
    private String lineToAttachTo2Id;

    @Schema(description = "attachment line id")
    private String attachedLineId;

    @Schema(description = "ID for the existing voltage level")
    private String voltageLevelId;

    @Schema(description = "bus bar section or bus id")
    private String bbsBusId;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine1Id;

    @Schema(description = "replacing line 1 name")
    private String replacingLine1Name;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine2Id;

    @Schema(description = "replacing line 2 name")
    private String replacingLine2Name;

    @Override
    public LinesAttachToSplitLinesEntity toEntity() {
        return LinesAttachToSplitLinesEntity.toEntity(
                getLineToAttachTo1Id(),
                getLineToAttachTo2Id(),
                getAttachedLineId(),
                getVoltageLevelId(),
                getBbsBusId(),
                getReplacingLine1Id(),
                getReplacingLine1Name(),
                getReplacingLine2Id(),
                getReplacingLine2Name()
        );
    }

    @Override
    public AbstractModification toModification() {
        return new LinesAttachToSplitLines(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.LINE_ATTACH_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter("linesAttachToSplitLines", "Lines attach to split lines");
    }
}
