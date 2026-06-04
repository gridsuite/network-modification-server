/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;

import jakarta.persistence.Column;
import jakarta.persistence.MappedSuperclass;
import org.gridsuite.modification.dto.ModificationDto;
import org.gridsuite.modification.model.InjectionCreationModel;

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

    @Column(name = "connectionName")
    private String connectionName;

    @Column(name = "connectionDirection")
    private ConnectablePosition.Direction connectionDirection;

    @Column(name = "connectionPosition")
    private Integer connectionPosition;

    @Column(name = "connected", columnDefinition = "boolean default true")
    private boolean terminalConnected;

    protected InjectionCreationEntity(ModificationDto injectionCreationInfos) {
        super(injectionCreationInfos);
        assignAttributes((InjectionCreationModel) injectionCreationInfos);
    }

    protected InjectionCreationEntity(InjectionCreationModel injectionCreationInfos) {
        super(injectionCreationInfos);
        assignAttributes(injectionCreationInfos);
    }

    @Override
    public void update(ModificationDto modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((InjectionCreationModel) modificationInfos);
    }

    private void assignAttributes(InjectionCreationModel injectionCreationInfos) {
        this.voltageLevelId = injectionCreationInfos.getVoltageLevelId();
        this.busOrBusbarSectionId = injectionCreationInfos.getBusOrBusbarSectionId();
        this.connectionName = injectionCreationInfos.getConnectionName();
        this.connectionPosition = injectionCreationInfos.getConnectionPosition();
        this.connectionDirection = injectionCreationInfos.getConnectionDirection();
        this.terminalConnected = injectionCreationInfos.isTerminalConnected();
    }
}
