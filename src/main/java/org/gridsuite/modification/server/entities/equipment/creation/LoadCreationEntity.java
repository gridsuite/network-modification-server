/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.LoadType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;

import jakarta.persistence.*;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

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

    @Column(name = "p0")
    private double p0;

    @Column(name = "q0")
    private double q0;

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
        p0 = loadCreationInfos.getP0();
        q0 = loadCreationInfos.getQ0();
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
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            // injection
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection())
            .connectionPosition(getConnectionPosition())
            .terminalConnected(isTerminalConnected())
            // load
            .loadType(getLoadType())
            .p0(getP0())
            .q0(getQ0())
            // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                getProperties().stream()
                    .map(FreePropertyEntity::toInfos)
                    .toList());
    }
}
