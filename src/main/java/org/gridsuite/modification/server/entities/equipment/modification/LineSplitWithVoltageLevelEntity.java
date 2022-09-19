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
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

import javax.persistence.*;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "LineSplitWithVoltageLevel")
public class LineSplitWithVoltageLevelEntity  extends ModificationEntity {

    @Column
    private String lineToSplitId;

    @Column
    private double percent;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
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
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        LineSplitWithVoltageLevelInfos splitWithVoltageLevelInfos = (LineSplitWithVoltageLevelInfos) modificationInfos;
        lineToSplitId = splitWithVoltageLevelInfos.getLineToSplitId();
        percent = splitWithVoltageLevelInfos.getPercent();
        if (splitWithVoltageLevelInfos.getMayNewVoltageLevelInfos() != null) {
            mayVoltageLevelCreation = VoltageLevelCreationEntity.toEntity(splitWithVoltageLevelInfos.getMayNewVoltageLevelInfos());
        }
        existingVoltageLevelId = splitWithVoltageLevelInfos.getExistingVoltageLevelId();
        bbsOrBusId = splitWithVoltageLevelInfos.getBbsOrBusId();
        newLine1Id = splitWithVoltageLevelInfos.getNewLine1Id();
        newLine1Name = splitWithVoltageLevelInfos.getNewLine1Name();
        newLine2Id = splitWithVoltageLevelInfos.getNewLine2Id();
        newLine2Name = splitWithVoltageLevelInfos.getNewLine2Name();
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

    public static LineSplitWithVoltageLevelEntity toEntity(String lineToSplitId, double percent,
        VoltageLevelCreationInfos mayVoltageLevelCreationInfos, String existingVoltageLevelId, String bbsOrBusId, String newLine1Id, String newLine1Name,
        String newLine2Id, String newLine2Name) {
        VoltageLevelCreationEntity voltageLevelCreationEntity = null;
        if (mayVoltageLevelCreationInfos != null) {
            voltageLevelCreationEntity = VoltageLevelCreationEntity.toEntity(mayVoltageLevelCreationInfos);
        }
        return new LineSplitWithVoltageLevelEntity(
            lineToSplitId, percent, voltageLevelCreationEntity, existingVoltageLevelId, bbsOrBusId, newLine1Id, newLine1Name, newLine2Id, newLine2Name
        );
    }

    @Override
    public void setIdsToNull() {
        if (this.getMayVoltageLevelCreation() != null) {
            this.getMayVoltageLevelCreation().setId(null);
        }
        super.setIdsToNull();
    }
}
