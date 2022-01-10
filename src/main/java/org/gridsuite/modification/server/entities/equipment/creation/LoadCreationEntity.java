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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.util.Set;

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

    public LoadCreationEntity(String equipmentId, String equipmentName, LoadType loadType, String voltageLevelId, String busOrBusbarSectionId,
                              double activePower, double reactivePower) {
        super(ModificationType.LOAD_CREATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.loadType = loadType;
        this.activePower = activePower;
        this.reactivePower = reactivePower;
    }

    @Override
    public LoadCreationInfos toModificationInfos() {
        return toLoadCreationInfosBuilder().build();
    }

    @Override
    public EquipmenModificationInfos toEquipmentModificationInfos(Set<String> uuids) {
        return toLoadCreationInfosBuilder().substationIds(uuids).build();
    }

    private LoadCreationInfos.LoadCreationInfosBuilder<?, ?> toLoadCreationInfosBuilder() {
        return LoadCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .active(isActive())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .loadType(getLoadType())
            .activePower(getActivePower())
            .reactivePower(getReactivePower());
    }
}
