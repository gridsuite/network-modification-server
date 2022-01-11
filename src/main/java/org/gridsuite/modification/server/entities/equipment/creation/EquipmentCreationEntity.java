/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class EquipmentCreationEntity extends EquipmentModificationEntity {
    @Column(name = "equipmentName")
    private String equipmentName;

    protected EquipmentCreationEntity(ModificationType modificationType, String equipmentId, String equipmentName) {
        super(equipmentId, modificationType);
        this.equipmentName = equipmentName;
    }
}
