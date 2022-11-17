/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "loadCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "loadCreation_id_fk_constraint"))
public class LoadCreationEntity extends InjectionCreationEntity {
    @Column(name = "loadType")
    private LoadType loadType;

    @Column(name = "activePower")
    private double activePower;

    @Column(name = "reactivePower")
    private double reactivePower;

    @Column(name = "connectionName")
    private String connectionName;

    @Column(name = "connectionDirection")
    private ConnectablePosition.Direction connectionDirection;

    public LoadCreationEntity(String equipmentId, String equipmentName, LoadType loadType, String voltageLevelId, String busOrBusbarSectionId,
                              double activePower, double reactivePower, String connectionName, ConnectablePosition.Direction connectionDirection) {
        super(ModificationType.LOAD_CREATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.loadType = loadType;
        this.activePower = activePower;
        this.reactivePower = reactivePower;
        this.connectionDirection = connectionDirection;
        this.connectionName = connectionName;
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        LoadCreationInfos loadCreationInfos = (LoadCreationInfos) modificationInfos;
        loadType = loadCreationInfos.getLoadType();
        activePower = loadCreationInfos.getActivePower();
        reactivePower = loadCreationInfos.getReactivePower();
        connectionName = loadCreationInfos.getConnectionName();
        connectionDirection = loadCreationInfos.getConnectionDirection();
    }

    @Override
    public LoadCreationInfos toModificationInfos() {
        return toLoadCreationInfosBuilder().build();
    }

    private LoadCreationInfos.LoadCreationInfosBuilder<?, ?> toLoadCreationInfosBuilder() {
        return LoadCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .loadType(getLoadType())
            .activePower(getActivePower())
            .reactivePower(getReactivePower())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection());
    }
}
