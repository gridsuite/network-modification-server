/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.model.OperatingStatusModificationModel;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "operatingStatusModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "operatingStatusModification_id_fk_constraint"))
public class OperatingStatusModificationEntity extends EquipmentModificationEntity {

    @Column(name = "action")
    @Enumerated(EnumType.STRING)
    private OperatingStatusModificationInfos.ActionType action;

    @Column
    private String energizedVoltageLevelId;

    public OperatingStatusModificationEntity(@NonNull ModificationInfos operatingStatusModificationInfos) {
        super(operatingStatusModificationInfos);
        assignAttributes((OperatingStatusModificationModel) operatingStatusModificationInfos.toModel());
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((OperatingStatusModificationModel) modificationInfos.toModel());
    }

    private void assignAttributes(OperatingStatusModificationModel operatingStatusModificationInfos) {
        action = operatingStatusModificationInfos.getAction();
        energizedVoltageLevelId = operatingStatusModificationInfos.getEnergizedVoltageLevelId();
    }

    @Override
    public OperatingStatusModificationInfos toModificationInfos() {
        var builder = OperatingStatusModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .action(getAction())
            .energizedVoltageLevelId(getEnergizedVoltageLevelId());
        return builder.build();
    }
}
