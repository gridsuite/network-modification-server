/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;

import javax.persistence.*;
import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "branchStatusModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "branchStatusModification_id_fk_constraint"))
public class BranchStatusModificationEntity extends EquipmentModificationEntity {

    @Column(name = "action")
    private String action;

    public BranchStatusModificationEntity(String lineId, BranchStatusModificationInfos.ActionType status) {
        super(lineId, ModificationType.BRANCH_STATUS);
        this.action = status.name();
    }

    @Override
    public BranchStatusModificationInfos toModificationInfos(Set<String> vlUuids) {
        return BranchStatusModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .substationIds(vlUuids)
            .equipmentId(getEquipmentId())
            .action(BranchStatusModificationInfos.ActionType.valueOf(getAction()))
            .build();
    }
}
