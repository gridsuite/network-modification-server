/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class BranchCreationEntity extends EquipmentCreationEntity {

    @Column(name = "voltageLevelId1")
    private String voltageLevelId1;

    @Column(name = "voltageLevelId2")
    private String voltageLevelId2;

    @Column(name = "busOrBusbarSectionId1")
    private String busOrBusbarSectionId1;

    @Column(name = "busOrBusbarSectionId2")
    private String busOrBusbarSectionId2;

    protected BranchCreationEntity(ModificationType modificationType, String equipmentId, String equipmentName, String voltageLevelId1, String voltageLevelId2, String busOrBusbarSectionId1, String busOrBusbarSectionId2) {
        super(modificationType, equipmentId, equipmentName);
        this.voltageLevelId1 = voltageLevelId1;
        this.voltageLevelId2 = voltageLevelId2;
        this.busOrBusbarSectionId1 = busOrBusbarSectionId1;
        this.busOrBusbarSectionId2 = busOrBusbarSectionId2;
    }
}
