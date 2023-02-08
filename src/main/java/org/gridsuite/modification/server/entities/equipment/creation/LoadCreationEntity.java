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
import lombok.NonNull;

import org.gridsuite.modification.server.dto.ConnectablePositionInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.*;

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

    @Column(name = "connectionPosition")
    private Integer connectionPosition;

    public LoadCreationEntity(@NonNull LoadCreationInfos loadCreationInfos) {
        super(loadCreationInfos);
        assignAttributes(loadCreationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LoadCreationInfos) modificationInfos);
    }

    private void assignAttributes(LoadCreationInfos loadCreationInfos) {
        loadType = loadCreationInfos.getLoadType();
        activePower = loadCreationInfos.getP0();
        reactivePower = loadCreationInfos.getQ0();
        connectionName =  loadCreationInfos.getPosition() != null ?
                loadCreationInfos.getPosition().getLabel() : null;
        connectionDirection = loadCreationInfos.getPosition() != null ?
                loadCreationInfos.getPosition().getDirection() : null;
        connectionPosition = loadCreationInfos.getPosition() != null ?
                loadCreationInfos.getPosition().getOrder() : null;
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
            .id(getEquipmentId())
            .name(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .loadType(getLoadType())
            .q0(getReactivePower())
            .p0(getActivePower())
            .position(ConnectablePositionInfos.builder()
                    .label(getConnectionName())
                    .direction(getConnectionDirection())
                    .order(getConnectionPosition()).build());
    }
}
