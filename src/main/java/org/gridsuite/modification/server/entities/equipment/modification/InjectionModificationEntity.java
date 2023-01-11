/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.InjectionModificationInfos;
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
public class InjectionModificationEntity extends BasicEquipmentModificationEntity {
    @Column(name = "voltageLevelIdValue")
    private String voltageLevelIdValue;

    @Column(name = "voltageLevelIdOp")
    @Enumerated(EnumType.STRING)
    private OperationType voltageLevelIdOp;

    @Column(name = "busOrBusbarSectionIdValue")
    private String busOrBusbarSectionIdValue;

    @Column(name = "busOrBusbarSectionIdOp")
    @Enumerated(EnumType.STRING)
    private OperationType busOrBusbarSectionIdOp;

    //TODO : Remove when LoadModification refactoring is done
    protected InjectionModificationEntity(ModificationType modificationType, String equipmentId,
                                          AttributeModification<String> equipmentName,
                                          AttributeModification<String> voltageLevelId,
                                          AttributeModification<String> busOrBusbarSectionId) {
        super(modificationType, equipmentId, equipmentName);
        this.voltageLevelIdValue = voltageLevelId != null ? voltageLevelId.getValue() : null;
        this.voltageLevelIdOp = voltageLevelId != null ? voltageLevelId.getOp() : null;
        this.busOrBusbarSectionIdValue = busOrBusbarSectionId != null ? busOrBusbarSectionId.getValue() : null;
        this.busOrBusbarSectionIdOp = busOrBusbarSectionId != null ? busOrBusbarSectionId.getOp() : null;
    }

    protected InjectionModificationEntity(InjectionModificationInfos modificationInfos) {
        super(modificationInfos);
        assignAttributes(modificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((InjectionModificationInfos) modificationInfos);
    }

    private void assignAttributes(InjectionModificationInfos modificationInfos) {
        this.voltageLevelIdValue = modificationInfos.getVoltageLevelId() != null ? modificationInfos.getVoltageLevelId().getValue() : null;
        this.voltageLevelIdOp = modificationInfos.getVoltageLevelId() != null ? modificationInfos.getVoltageLevelId().getOp() : null;
        this.busOrBusbarSectionIdValue = modificationInfos.getBusOrBusbarSectionId() != null ? modificationInfos.getBusOrBusbarSectionId().getValue() : null;
        this.busOrBusbarSectionIdOp = modificationInfos.getBusOrBusbarSectionId() != null ? modificationInfos.getBusOrBusbarSectionId().getOp() : null;
    }
}
