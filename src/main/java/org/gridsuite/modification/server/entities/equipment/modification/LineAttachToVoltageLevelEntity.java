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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.mapper.MappingUtil;

import jakarta.persistence.*;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ATTACH_DESCRIPTION_ERROR;

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

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
    private VoltageLevelCreationEntity mayVoltageLevelCreation;

    @Column
    private String existingVoltageLevelId;

    @Column
    private String bbsOrBusId;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true)
    private LineCreationEntity lineCreation;

    @Column
    private String newLine1Id;

    @Column
    private String newLine1Name;

    @Column
    private String newLine2Id;

    @Column
    private String newLine2Name;

    public LineAttachToVoltageLevelEntity(@NonNull LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        super(lineAttachToVoltageLevelInfos);
        assignAttributes(lineAttachToVoltageLevelInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LineAttachToVoltageLevelInfos) modificationInfos);
    }

    private void assignAttributes(LineAttachToVoltageLevelInfos lineAttachToVoltageLevelInfos) {
        lineToAttachToId = lineAttachToVoltageLevelInfos.getLineToAttachToId();
        percent = lineAttachToVoltageLevelInfos.getPercent();
        attachmentPointId = lineAttachToVoltageLevelInfos.getAttachmentPointId();
        attachmentPointName = lineAttachToVoltageLevelInfos.getAttachmentPointName();
        mayVoltageLevelCreation = null; // Needed for the update
        if (lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos() != null) {
            mayVoltageLevelCreation = (VoltageLevelCreationEntity) MappingUtil.mapToEntity(lineAttachToVoltageLevelInfos.getMayNewVoltageLevelInfos());
        }
        existingVoltageLevelId = lineAttachToVoltageLevelInfos.getExistingVoltageLevelId();
        bbsOrBusId = lineAttachToVoltageLevelInfos.getBbsOrBusId();
        if (lineAttachToVoltageLevelInfos.getAttachmentLine() == null) {
            throw new NetworkModificationException(LINE_ATTACH_DESCRIPTION_ERROR, "Missing required attachment line description");
        }
        lineCreation = new LineCreationEntity(lineAttachToVoltageLevelInfos.getAttachmentLine());
        newLine1Id = lineAttachToVoltageLevelInfos.getNewLine1Id();
        newLine1Name = lineAttachToVoltageLevelInfos.getNewLine1Name();
        newLine2Id = lineAttachToVoltageLevelInfos.getNewLine2Id();
        newLine2Name = lineAttachToVoltageLevelInfos.getNewLine2Name();
    }

    @Override
    public LineAttachToVoltageLevelInfos toModificationInfos() {
        return toLineAttachToVoltageLevelInfosBuilder().build();
    }

    private LineAttachToVoltageLevelInfos.LineAttachToVoltageLevelInfosBuilder<?, ?> toLineAttachToVoltageLevelInfosBuilder() {
        return LineAttachToVoltageLevelInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
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
}
