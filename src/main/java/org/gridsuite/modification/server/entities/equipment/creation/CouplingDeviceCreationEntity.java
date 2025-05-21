/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.CouplingDeviceCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

/**
 * @author etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "couplingDeviceCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "couplingDeviceCreation_id_fk_constraint"))
public class CouplingDeviceCreationEntity extends ModificationEntity {
    @Column
    private String voltageLevelId;
    @Column
    private String busOrBbsId1;
    @Column
    private String busOrBbsId2;

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((CouplingDeviceCreationInfos) modificationInfos);
    }

    private void assignAttributes(CouplingDeviceCreationInfos couplingDeviceCreationInfos) {
        this.voltageLevelId = couplingDeviceCreationInfos.getVoltageLevelId();
        this.busOrBbsId1 = couplingDeviceCreationInfos.getBusOrBbsId1();
        this.busOrBbsId2 = couplingDeviceCreationInfos.getBusOrBbsId2();
    }

    public CouplingDeviceCreationEntity(CouplingDeviceCreationInfos couplingDeviceCreationInfos) {
        super(couplingDeviceCreationInfos);
        assignAttributes(couplingDeviceCreationInfos);
    }

    @Override
    public CouplingDeviceCreationInfos toModificationInfos() {
        return toCouplingDeviceCreationInfos().build();
    }

    private CouplingDeviceCreationInfos.CouplingDeviceCreationInfosBuilder<?, ?> toCouplingDeviceCreationInfos() {
        return CouplingDeviceCreationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .voltageLevelId(getVoltageLevelId())
            .busOrBbsId1(getBusOrBbsId1())
            .busOrBbsId2(getBusOrBbsId2());
    }
}
