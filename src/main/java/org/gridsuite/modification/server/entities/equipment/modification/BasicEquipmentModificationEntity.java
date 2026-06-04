/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.MappedSuperclass;
import org.gridsuite.modification.dto.ModificationDto;
import org.gridsuite.modification.model.BasicEquipmentModificationModel;
import org.gridsuite.modification.model.constants.OperationType;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class BasicEquipmentModificationEntity extends EquipmentModificationEntity {
    @Column(name = "equipmentNameValue")
    private String equipmentNameValue;

    @Column(name = "equipmentNameOp")
    @Enumerated(EnumType.STRING)
    private OperationType equipmentNameOp;

    protected BasicEquipmentModificationEntity(ModificationDto modificationInfos) {
        super(modificationInfos);
        assignAttributes((BasicEquipmentModificationModel) modificationInfos);
    }

    @Override
    public void update(ModificationDto modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BasicEquipmentModificationModel) modificationInfos);
    }

    private void assignAttributes(BasicEquipmentModificationModel modificationInfos) {
        this.equipmentNameValue = modificationInfos.getEquipmentName() != null ? modificationInfos.getEquipmentName().getValue() : null;
        this.equipmentNameOp = modificationInfos.getEquipmentName() != null ? modificationInfos.getEquipmentName().getOp() : null;
    }
}
