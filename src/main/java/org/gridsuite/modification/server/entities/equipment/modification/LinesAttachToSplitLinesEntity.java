/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "LinesAttachToSplitLines")
public class LinesAttachToSplitLinesEntity extends ModificationEntity {

    @Column
    private String lineToAttachTo1Id;

    @Column
    private String lineToAttachTo2Id;

    @Column
    private String attachedLineId;

    @Column
    private String voltageLevelId;

    @Column
    private String bbsBusId;

    @Column
    private String replacingLine1Id;

    @Column
    private String replacingLine1Name;

    @Column
    private String replacingLine2Id;

    @Column
    private String replacingLine2Name;

    public LinesAttachToSplitLinesEntity(@NonNull LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        super(ModificationType.LINES_ATTACH_TO_SPLIT_LINES);
        assignAttributes(linesAttachToSplitLinesInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LinesAttachToSplitLinesInfos) modificationInfos);
    }

    private void assignAttributes(LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos) {
        lineToAttachTo1Id = linesAttachToSplitLinesInfos.getLineToAttachTo1Id();
        lineToAttachTo2Id = linesAttachToSplitLinesInfos.getLineToAttachTo2Id();
        attachedLineId = linesAttachToSplitLinesInfos.getAttachedLineId();
        voltageLevelId = linesAttachToSplitLinesInfos.getVoltageLevelId();
        bbsBusId = linesAttachToSplitLinesInfos.getBbsBusId();
        replacingLine1Id = linesAttachToSplitLinesInfos.getReplacingLine1Id();
        replacingLine1Name = linesAttachToSplitLinesInfos.getReplacingLine1Name();
        replacingLine2Id = linesAttachToSplitLinesInfos.getReplacingLine2Id();
        replacingLine2Name = linesAttachToSplitLinesInfos.getReplacingLine2Name();
    }

    @Override
    public LinesAttachToSplitLinesInfos toModificationInfos() {
        return toLinesAttachToSplitLinesInfosBuilder().build();
    }

    private LinesAttachToSplitLinesInfos.LinesAttachToSplitLinesInfosBuilder toLinesAttachToSplitLinesInfosBuilder() {
        return LinesAttachToSplitLinesInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .lineToAttachTo1Id(getLineToAttachTo1Id())
                .lineToAttachTo2Id(getLineToAttachTo2Id())
                .attachedLineId(getAttachedLineId())
                .voltageLevelId(getVoltageLevelId())
                .bbsBusId(getBbsBusId())
                .replacingLine1Id(getReplacingLine1Id())
                .replacingLine1Name(getReplacingLine1Name())
                .replacingLine2Id(getReplacingLine2Id())
                .replacingLine2Name(getReplacingLine2Name());
    }
}
