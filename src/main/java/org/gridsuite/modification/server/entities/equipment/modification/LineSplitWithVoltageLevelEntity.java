/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table
public class LineSplitWithVoltageLevelEntity  extends ModificationEntity {

    @Column
    private String lineToSplitId;

    @Column
    private double percent;

    @OneToOne(cascade = CascadeType.ALL)
    private VoltageLevelCreationEntity mayVoltageLevelCreation;

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

    public LineSplitWithVoltageLevelEntity(String lineToSplitId, double percent,
        VoltageLevelCreationEntity mayVoltageLevelCreation,
        String existingVoltageLevelId, String bbsOrBusId,
        String newLine1Id, String newLine1Name, String newLine2Id, String newLine2Name) {
        super(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL);

        this.lineToSplitId = lineToSplitId;
        this.percent = percent;
        this.mayVoltageLevelCreation = mayVoltageLevelCreation;
        this.existingVoltageLevelId = existingVoltageLevelId;
        this.bbsOrBusId = bbsOrBusId;
        this.newLine1Id = newLine1Id;
        this.newLine1Name = newLine1Name;
        this.newLine2Id = newLine2Id;
        this.newLine2Name = newLine2Name;
    }

    @Override
    public LineSplitWithVoltageLevelInfos toModificationInfos() {
        return toLineSplitWithVoltageLevelInfosBuilder().build();
    }

    public LineSplitWithVoltageLevelInfos toLineSplitWithVoltageLevelInfos() {
        return toLineSplitWithVoltageLevelInfosBuilder().build();
    }

    private LineSplitWithVoltageLevelInfos.LineSplitWithVoltageLevelInfosBuilder<?, ?> toLineSplitWithVoltageLevelInfosBuilder() {
        return LineSplitWithVoltageLevelInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .lineToSplitId(getLineToSplitId())
            .percent(getPercent())
            .mayNewVoltageLevelInfos(mayVoltageLevelCreation == null ? null : mayVoltageLevelCreation.toVoltageLevelCreationInfos())
            .existingVoltageLevelId(getExistingVoltageLevelId())
            .bbsOrBusId(getBbsOrBusId())
            .newLine1Id(getNewLine1Id())
            .newLine1Name(getNewLine1Name())
            .newLine2Id(getNewLine2Id())
            .newLine2Name(getNewLine2Name());
    }
}
