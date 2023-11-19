/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.OperationalStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import jakarta.persistence.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "operationalStatusModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "operationalStatusModification_id_fk_constraint"))
public class OperationalStatusModificationEntity extends EquipmentModificationEntity {

    @Column(name = "action")
    @Enumerated(EnumType.STRING)
    private OperationalStatusModificationInfos.ActionType action;

    @Column
    private String energizedVoltageLevelId;

    public OperationalStatusModificationEntity(@NonNull OperationalStatusModificationInfos operationalStatusModificationInfos) {
        super(operationalStatusModificationInfos);
        assignAttributes(operationalStatusModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((OperationalStatusModificationInfos) modificationInfos);
    }

    private void assignAttributes(OperationalStatusModificationInfos operationalStatusModificationInfos) {
        action = operationalStatusModificationInfos.getAction();
        energizedVoltageLevelId = operationalStatusModificationInfos.getEnergizedVoltageLevelId();
    }

    @Override
    public OperationalStatusModificationInfos toModificationInfos() {
        return OperationalStatusModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .equipmentId(getEquipmentId())
            .action(getAction())
            .energizedVoltageLevelId(getEnergizedVoltageLevelId())
            .build();
    }
}
