/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;
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

    public LinesAttachToSplitLinesEntity(String lineToAttachTo1Id, String lineToAttachTo2Id, String attachedLineId, String voltageLevelId, String bbsBusId, String replacingLine1Id, String replacingLine1Name, String replacingLine2Id, String replacingLine2Name) {
        super(ModificationType.LINES_ATTACH_TO_SPLIT_LINES);
        this.lineToAttachTo1Id = lineToAttachTo1Id;
        this.lineToAttachTo2Id = lineToAttachTo2Id;
        this.attachedLineId = attachedLineId;
        this.voltageLevelId = voltageLevelId;
        this.bbsBusId = bbsBusId;
        this.replacingLine1Id = replacingLine1Id;
        this.replacingLine1Name = replacingLine1Name;
        this.replacingLine2Id = replacingLine2Id;
        this.replacingLine2Name = replacingLine2Name;
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

    public static LinesAttachToSplitLinesEntity toEntity(String lineToAttachTo1Id,
                                                          String lineToAttachTo2Id, String attachedLineId,
                                                          String voltageLevelId, String bbsBusId,
                                                          String replacingLine1Id, String replacingLine1Name, String replacingLine2Id, String replacingLine2Name) {

        return new LinesAttachToSplitLinesEntity(
            lineToAttachTo1Id, lineToAttachTo2Id, attachedLineId, voltageLevelId,
            bbsBusId, replacingLine1Id, replacingLine1Name, replacingLine2Id, replacingLine2Name
        );
    }
}
