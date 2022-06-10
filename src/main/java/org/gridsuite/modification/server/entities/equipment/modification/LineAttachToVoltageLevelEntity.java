/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

import javax.persistence.*;

/**
 * @author Nicolas NOIR <nicolas.noir at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "LineAttachToVoltageLevel")
public class LineAttachToVoltageLevelEntity extends ModificationEntity {

    @Column
    private String lineToAttachToId;

    @Column
    private double percent;

    @Column
    private String attachmentPointId;

    @Column
    private String attachmentPointName;

    @OneToOne(cascade = CascadeType.ALL)
    private VoltageLevelCreationEntity mayVoltageLevelCreation;

    @Column
    private String existingVoltageLevelId;

    @Column
    private String bbsOrBusId;

    @OneToOne(cascade = CascadeType.ALL)
    private LineCreationEntity lineCreation;

    @Column
    private String newLine1Id;

    @Column
    private String newLine1Name;

    @Column
    private String newLine2Id;

    @Column
    private String newLine2Name;

    public LineAttachToVoltageLevelEntity(String lineToAttachToId, double percent,
                                          String attachmentPointId, String attachmentPointName,
                                          VoltageLevelCreationEntity mayVoltageLevelCreation,
                                          String existingVoltageLevelId, String bbsOrBusId,
                                          LineCreationEntity lineCreation,
                                          String newLine1Id, String newLine1Name, String newLine2Id, String newLine2Name) {
        super(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL);

        this.lineToAttachToId = lineToAttachToId;
        this.percent = percent;
        this.attachmentPointId = attachmentPointId;
        this.attachmentPointName = attachmentPointName;
        this.mayVoltageLevelCreation = mayVoltageLevelCreation;
        this.existingVoltageLevelId = existingVoltageLevelId;
        this.bbsOrBusId = bbsOrBusId;
        this.lineCreation = lineCreation;
        this.newLine1Id = newLine1Id;
        this.newLine1Name = newLine1Name;
        this.newLine2Id = newLine2Id;
        this.newLine2Name = newLine2Name;
    }

    @Override
    public LineAttachToVoltageLevelInfos toModificationInfos() {
        return toLineAttachToVoltageLevelInfosBuilder().build();
    }

    public LineAttachToVoltageLevelInfos toLineAttachToVoltageLevelInfos() {
        return toLineAttachToVoltageLevelInfosBuilder().build();
    }

    private LineAttachToVoltageLevelInfos.LineAttachToVoltageLevelInfosBuilder toLineAttachToVoltageLevelInfosBuilder() {
        return LineAttachToVoltageLevelInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .lineToAttachToId(getLineToAttachToId())
                .percent(getPercent())
                .attachmentPointId(getAttachmentPointId())
                .attachmentPointName(getAttachmentPointName())
                .existingVoltageLevelId(getExistingVoltageLevelId())
                .mayNewVoltageLevelInfos(mayVoltageLevelCreation == null ? null : mayVoltageLevelCreation.toVoltageLevelCreationInfos())
                .bbsOrBusId(getBbsOrBusId())
                .attachmentLine(lineCreation.toModificationInfos())
                .newLine1Id(getNewLine1Id())
                .newLine1Name(getNewLine1Name())
                .newLine2Id(getNewLine2Id())
                .newLine2Name(getNewLine2Name());
    }

    public static LineAttachToVoltageLevelEntity toEntity(String lineToAttachToId, double percent,
                                                          String attachmentPointId, String attachmentPointName,
                                                          VoltageLevelCreationInfos mayVoltageLevelCreationInfos,
                                                          String existingVoltageLevelId, String bbsOrBusId,
                                                          LineCreationInfos lineCreationInfos,
                                                          String newLine1Id, String newLine1Name, String newLine2Id, String newLine2Name) {
        VoltageLevelCreationEntity voltageLevelCreationEntity = null;
        if (mayVoltageLevelCreationInfos != null) {
            voltageLevelCreationEntity = VoltageLevelCreationEntity.toEntity(mayVoltageLevelCreationInfos);
        }

        LineCreationEntity lineCreationEntity = null;
        if (lineCreationInfos != null) {
            lineCreationEntity = LineCreationEntity.toEntity(lineCreationInfos);
        }

        return new LineAttachToVoltageLevelEntity(
                lineToAttachToId, percent, attachmentPointId, attachmentPointName, voltageLevelCreationEntity, existingVoltageLevelId,
                bbsOrBusId, lineCreationEntity, newLine1Id, newLine1Name, newLine2Id, newLine2Name
        );
    }

}
