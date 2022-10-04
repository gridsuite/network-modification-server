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
    private String existingVoltageLevelId;

    @Column
    private String bbsOrBusId;

    @Column
    private String newLine1Id;

    @Column
    private String newLine1Name;

    @Column
    private String newLine2Id;

    @Column
    private String newLine2Name;

    public LinesAttachToSplitLinesEntity(String lineToAttachTo1Id, String lineToAttachTo2Id, String attachedLineId, String existingVoltageLevelId, String bbsOrBusId, String newLine1Id, String newLine1Name, String newLine2Id, String newLine2Name) {
        super(ModificationType.LINES_ATTACH_TO_SPLIT_LINES);
        this.lineToAttachTo1Id = lineToAttachTo1Id;
        this.lineToAttachTo2Id = lineToAttachTo2Id;
        this.attachedLineId = attachedLineId;
        this.existingVoltageLevelId = existingVoltageLevelId;
        this.bbsOrBusId = bbsOrBusId;
        this.newLine1Id = newLine1Id;
        this.newLine1Name = newLine1Name;
        this.newLine2Id = newLine2Id;
        this.newLine2Name = newLine2Name;
    }

    @Override
    public LinesAttachToSplitLinesInfos toModificationInfos() {
        return toLinesAttachToSplitLinesInfosBuilder().build();
    }

    public LinesAttachToSplitLinesInfos toLinesAttachToSplitLinesInfos() {
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
                .voltageLevelId(getExistingVoltageLevelId())
                .bbsBusId(getBbsOrBusId())
                .replacingLine1Id(getNewLine1Id())
                .replacingLine1Name(getNewLine1Name())
                .replacingLine2Id(getNewLine2Id())
                .replacingLine2Name(getNewLine2Name());
    }

    public static LinesAttachToSplitLinesEntity toEntity(String lineToAttachTo1Id,
                                                          String lineToAttachTo2Id, String attachedLineId,
                                                          String existingVoltageLevelId, String bbsOrBusId,
                                                          String newLine1Id, String newLine1Name, String newLine2Id, String newLine2Name) {

        return new LinesAttachToSplitLinesEntity(
            lineToAttachTo1Id, lineToAttachTo2Id, attachedLineId, existingVoltageLevelId,
            bbsOrBusId, newLine1Id, newLine1Name, newLine2Id, newLine2Name
        );
    }
}
