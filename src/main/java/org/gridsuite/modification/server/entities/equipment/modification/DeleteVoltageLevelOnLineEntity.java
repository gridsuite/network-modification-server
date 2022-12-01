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
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
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
@Table(name = "DeleteVoltageLevelOnLine")
public class DeleteVoltageLevelOnLineEntity extends ModificationEntity {

    @Column
    private String lineToAttachTo1Id;

    @Column
    private String lineToAttachTo2Id;

    @Column
    private String attachedLineId;

    @Column
    private String replacingLine1Id;

    @Column
    private String replacingLine1Name;

    public DeleteVoltageLevelOnLineEntity(String lineToAttachTo1Id, String lineToAttachTo2Id, String attachedLineId, String replacingLine1Id, String replacingLine1Name) {
        super(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE);
        this.lineToAttachTo1Id = lineToAttachTo1Id;
        this.lineToAttachTo2Id = lineToAttachTo2Id;
        this.attachedLineId = attachedLineId;
        this.replacingLine1Id = replacingLine1Id;
        this.replacingLine1Name = replacingLine1Name;
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = (DeleteVoltageLevelOnLineInfos) modificationInfos;

        lineToAttachTo1Id = deleteVoltageLevelOnLineInfos.getLineToAttachTo1Id();
        lineToAttachTo2Id = deleteVoltageLevelOnLineInfos.getLineToAttachTo2Id();
        attachedLineId = deleteVoltageLevelOnLineInfos.getAttachedLineId();
        replacingLine1Id = deleteVoltageLevelOnLineInfos.getReplacingLine1Id();
        replacingLine1Name = deleteVoltageLevelOnLineInfos.getReplacingLine1Name();
    }

    @Override
    public DeleteVoltageLevelOnLineInfos toModificationInfos() {
        return toDeleteVoltageLevelOnLineInfosBuilder().build();
    }

    private DeleteVoltageLevelOnLineInfos.DeleteVoltageLevelOnLineInfosBuilder<?, ?> toDeleteVoltageLevelOnLineInfosBuilder() {
        return DeleteVoltageLevelOnLineInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .lineToAttachTo1Id(getLineToAttachTo1Id())
                .lineToAttachTo2Id(getLineToAttachTo2Id())
                .attachedLineId(getAttachedLineId())
                .replacingLine1Id(getReplacingLine1Id())
                .replacingLine1Name(getReplacingLine1Name());
    }

    public static DeleteVoltageLevelOnLineEntity toEntity(String lineToAttachTo1Id, String lineToAttachTo2Id, String attachedLineId, String replacingLine1Id, String replacingLine1Name) {
        return new DeleteVoltageLevelOnLineEntity(lineToAttachTo1Id, lineToAttachTo2Id, attachedLineId, replacingLine1Id, replacingLine1Name);
    }
}
