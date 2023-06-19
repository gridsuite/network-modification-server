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
    private String replacingLine1Id;

    @Column
    private String replacingLine1Name;

    public DeleteVoltageLevelOnLineEntity(@NonNull DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos) {
        super(deleteVoltageLevelOnLineInfos);
        assignAttributes(deleteVoltageLevelOnLineInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((DeleteVoltageLevelOnLineInfos) modificationInfos);
    }

    @Override
    public DeleteVoltageLevelOnLineInfos toModificationInfos() {
        return toDeleteVoltageLevelOnLineInfosBuilder().build();
    }

    private DeleteVoltageLevelOnLineInfos.DeleteVoltageLevelOnLineInfosBuilder<?, ?> toDeleteVoltageLevelOnLineInfosBuilder() {
        return DeleteVoltageLevelOnLineInfos
                .builder()
                .uuid(getId())
                .groupUuid(getGroup().getId())
                .date(getDate())
                .lineToAttachTo1Id(getLineToAttachTo1Id())
                .lineToAttachTo2Id(getLineToAttachTo2Id())
                .replacingLine1Id(getReplacingLine1Id())
                .replacingLine1Name(getReplacingLine1Name());
    }

    private void assignAttributes(DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos) {
        lineToAttachTo1Id = deleteVoltageLevelOnLineInfos.getLineToAttachTo1Id();
        lineToAttachTo2Id = deleteVoltageLevelOnLineInfos.getLineToAttachTo2Id();
        replacingLine1Id = deleteVoltageLevelOnLineInfos.getReplacingLine1Id();
        replacingLine1Name = deleteVoltageLevelOnLineInfos.getReplacingLine1Name();
    }
}
