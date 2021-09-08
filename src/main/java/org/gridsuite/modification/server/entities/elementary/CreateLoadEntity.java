/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.elementary;

import com.powsybl.iidm.network.LoadType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
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
@Table(name = "createLoad")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "createLoad_id_fk_constraint"))
public class CreateLoadEntity extends CreateEquipmentEntity {
    @Column(name = "loadType")
    private LoadType loadType;

    @Column(name = "activePower")
    private double activePower;

    @Column(name = "reactivePower")
    private double reactivePower;

    public CreateLoadEntity(String equipmentId, String equipmentName, LoadType loadType, String voltageLevelId, String busId,
                            double activePower, double reactivePower) {
        super(ModificationType.LOAD_CREATION, equipmentId, equipmentName, voltageLevelId, busId);
        this.loadType = loadType;
        this.activePower = activePower;
        this.reactivePower = reactivePower;
    }

    public LoadCreationInfos toLoadCreationInfos() {
        return toLoadCreationInfosBuilder().build();
    }

    public LoadCreationInfos toLoadCreationInfos(Set<String> substationIds) {
        return (LoadCreationInfos) toLoadCreationInfosBuilder().substationIds(substationIds).build();
    }

    private LoadCreationInfos.LoadCreationInfosBuilder toLoadCreationInfosBuilder() {
        return LoadCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busId(getBusId())
            .loadType(getLoadType())
            .activePower(getActivePower())
            .reactivePower(getReactivePower());
    }
}
