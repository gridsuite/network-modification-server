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
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class EquipmentModificationEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    protected EquipmentModificationEntity(String equipmentId, ModificationType modificationType) {
        super(modificationType);
        this.equipmentId = equipmentId;
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        EquipmentModificationInfos equipmentModificationInfos = (EquipmentModificationInfos) modificationInfos;
        equipmentId = equipmentModificationInfos.getEquipmentId();
    }
}
