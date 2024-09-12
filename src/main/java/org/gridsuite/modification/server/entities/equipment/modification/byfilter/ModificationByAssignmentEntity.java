/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.simple.AssignmentEntity;

import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@NoArgsConstructor
@Entity
@Table(name = "modificationByAssignment")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "modificationByAssignment_id_fk_constraint"))
public class ModificationByAssignmentEntity extends ModificationEntity {
    @Enumerated(EnumType.STRING)
    @Column
    private IdentifiableType equipmentType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "modification_by_assignment_id",
            foreignKey = @ForeignKey(name = "modification_by_assignment_id_fk"))
    private List<AssignmentEntity> assignmentEntities;

    public ModificationByAssignmentEntity(ModificationByAssignmentInfos modificationByAssignmentInfos) {
        super(modificationByAssignmentInfos);
        assignAttributes(modificationByAssignmentInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ModificationByAssignmentInfos) modificationInfos);
    }

    private void assignAttributes(ModificationByAssignmentInfos modificationByAssignmentInfos) {
        this.equipmentType = modificationByAssignmentInfos.getEquipmentType();
        if (assignmentEntities == null) {
            assignmentEntities = modificationByAssignmentInfos.getAssignmentInfosList()
                    .stream()
                    .map(AssignmentInfos::toEntity)
                    .toList();
        } else {
            assignmentEntities.clear();
            assignmentEntities.addAll(modificationByAssignmentInfos.getAssignmentInfosList()
                    .stream()
                    .map(AssignmentInfos::toEntity)
                    .toList());
        }
    }

    @Override
    public ModificationByAssignmentInfos toModificationInfos() {
        return toModificationByAssignmentInfosBuilder().build();
    }

    private ModificationByAssignmentInfos.ModificationByAssignmentInfosBuilder<?, ?> toModificationByAssignmentInfosBuilder() {
        return ModificationByAssignmentInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .equipmentType(equipmentType)
            .assignmentInfosList(assignmentEntities.stream()
                .map(AssignmentEntity::toAssignmentInfos)
                .toList()
            );
    }
}
