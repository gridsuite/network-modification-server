/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.elementary;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmentCreationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import java.util.Set;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class CreateEquipmentEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Column(name = "equipmentName")
    private String equipmentName;

    @Column(name = "voltageLevelId")
    private String voltageLevelId;

    @Column(name = "busId")
    private String busId;

    protected CreateEquipmentEntity(ModificationType modificationType, String equipmentId, String equipmentName, String voltageLevelId, String busId) {
        super(modificationType);
        this.equipmentId = equipmentId;
        this.equipmentName = equipmentName;
        this.voltageLevelId = voltageLevelId;
        this.busId = busId;
    }

    public EquipmentCreationInfos toEquipmentCreationInfos() {
        return toEquipmentCreationInfosBuilder().build();
    }

    public EquipmentCreationInfos toEquipmentCreationInfos(Set<String> substationIds) {
        return (EquipmentCreationInfos) toEquipmentCreationInfosBuilder().substationIds(substationIds).build();
    }

    private EquipmentCreationInfos.EquipmentCreationInfosBuilder toEquipmentCreationInfosBuilder() {
        return EquipmentCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busId(getBusId());
    }
}
