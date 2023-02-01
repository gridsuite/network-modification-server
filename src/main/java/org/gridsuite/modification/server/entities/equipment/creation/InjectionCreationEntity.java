/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.InjectionCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class InjectionCreationEntity extends EquipmentCreationEntity {
    @Column(name = "voltageLevelId")
    private String voltageLevelId;

    @Column(name = "busOrBusbarSectionId")
    private String busOrBusbarSectionId;

    protected InjectionCreationEntity(InjectionCreationInfos injectionCreationInfos) {
        super(injectionCreationInfos);
        assignAttributes(injectionCreationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((InjectionCreationInfos) modificationInfos);
    }

    private void assignAttributes(InjectionCreationInfos injectionCreationInfos) {
        this.voltageLevelId = injectionCreationInfos.getVoltageLevelId();
        this.busOrBusbarSectionId = injectionCreationInfos.getBusOrBusbarSectionId();
    }
}
