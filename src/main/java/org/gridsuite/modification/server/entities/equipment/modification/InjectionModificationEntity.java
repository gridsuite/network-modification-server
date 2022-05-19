/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
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
@EqualsAndHashCode
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
}
