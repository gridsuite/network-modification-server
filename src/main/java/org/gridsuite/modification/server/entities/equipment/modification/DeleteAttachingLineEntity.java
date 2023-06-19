/*
* Copyright (c) 2022, RTE (http://www.rte-france.com)
* This Source Code Form is subject to the terms of the Mozilla Public
* License, v. 2.0. If a copy of the MPL was not distributed with this
* file, You can obtain one at http://mozilla.org/MPL/2.0/.
*/
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;
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
@Table(name = "DeleteAttachingLine")
public class DeleteAttachingLineEntity extends ModificationEntity {

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

    public DeleteAttachingLineEntity(DeleteAttachingLineInfos deleteAttachingLineInfos) {
        super(deleteAttachingLineInfos);
        assignAttributes(deleteAttachingLineInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((DeleteAttachingLineInfos) modificationInfos);
    }

    @Override
    public DeleteAttachingLineInfos toModificationInfos() {
        return toDeleteAttachingLineInfosBuilder().build();
    }

    private DeleteAttachingLineInfos.DeleteAttachingLineInfosBuilder<?, ?> toDeleteAttachingLineInfosBuilder() {
        return DeleteAttachingLineInfos
                .builder()
                .uuid(getId())
                .groupUuid(getGroup().getId())
                .date(getDate())
                .lineToAttachTo1Id(getLineToAttachTo1Id())
                .lineToAttachTo2Id(getLineToAttachTo2Id())
                .attachedLineId(getAttachedLineId())
                .replacingLine1Id(getReplacingLine1Id())
                .replacingLine1Name(getReplacingLine1Name());
    }

    private void assignAttributes(DeleteAttachingLineInfos deleteAttachingLineInfos) {
        lineToAttachTo1Id = deleteAttachingLineInfos.getLineToAttachTo1Id();
        lineToAttachTo2Id = deleteAttachingLineInfos.getLineToAttachTo2Id();
        attachedLineId = deleteAttachingLineInfos.getAttachedLineId();
        replacingLine1Id = deleteAttachingLineInfos.getReplacingLine1Id();
        replacingLine1Name = deleteAttachingLineInfos.getReplacingLine1Name();
    }
}
