/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.BasicEquipmentModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperationType;

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.MappedSuperclass;

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

    protected BasicEquipmentModificationEntity(BasicEquipmentModificationInfos modificationInfos) {
        super(modificationInfos);
        assignAttributes(modificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BasicEquipmentModificationInfos) modificationInfos);
    }

    private void assignAttributes(BasicEquipmentModificationInfos modificationInfos) {
        this.equipmentNameValue = modificationInfos.getEquipmentName() != null ? modificationInfos.getEquipmentName().getValue() : null;
        this.equipmentNameOp = modificationInfos.getEquipmentName() != null ? modificationInfos.getEquipmentName().getOp() : null;
    }
}
