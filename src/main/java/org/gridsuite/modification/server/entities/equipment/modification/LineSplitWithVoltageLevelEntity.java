/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

import jakarta.persistence.*;

import java.io.UncheckedIOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "LineSplitWithVoltageLevel")
public class LineSplitWithVoltageLevelEntity extends ModificationEntity {

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

    public LineSplitWithVoltageLevelEntity(@NonNull LineSplitWithVoltageLevelInfos splitWithVoltageLevelInfos) {
        super(splitWithVoltageLevelInfos);
        assignAttributes(splitWithVoltageLevelInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LineSplitWithVoltageLevelInfos) modificationInfos);
    }

    private void assignAttributes(LineSplitWithVoltageLevelInfos splitWithVoltageLevelInfos) {
        lineToSplitId = splitWithVoltageLevelInfos.getLineToSplitId();
        percent = splitWithVoltageLevelInfos.getPercent();
        mayVoltageLevelCreation = null; // Need for the update
        if (splitWithVoltageLevelInfos.getMayNewVoltageLevelInfos() != null) {
            mayVoltageLevelCreation = splitWithVoltageLevelInfos.getMayNewVoltageLevelInfos().toEntity();
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

    @Override
    public void getAdditionalInfosForMetadata(ModificationInfos modificationInfos) {
        super.getAdditionalInfosForMetadata(modificationInfos);
        try {
            Map<String, String> messageValuesMap = new HashMap<>();
            messageValuesMap.put("lineToSplitId", ((LineSplitWithVoltageLevelInfos) modificationInfos).getLineToSplitId());
            ObjectMapper objectMapper = new ObjectMapper();
            this.setMessageValues(objectMapper.writeValueAsString(messageValuesMap));
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
    }

    private LineSplitWithVoltageLevelInfos.LineSplitWithVoltageLevelInfosBuilder<?, ?> toLineSplitWithVoltageLevelInfosBuilder() {
        return LineSplitWithVoltageLevelInfos
            .builder()
            .uuid(getId())
            .date(getDate())
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
