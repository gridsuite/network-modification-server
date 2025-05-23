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
import org.gridsuite.modification.dto.CreateCouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

/**
 * @author etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "create_coupling_device")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "create_coupling_device_id_fk_constraint"))
public class CreateCouplingDeviceEntity extends ModificationEntity {
    @Column(name = "voltage_level_id")
    private String voltageLevelId;
    @Column(name = "bus_or_bbs_id1")
    private String busOrBbsId1;
    @Column(name = "bus_or_bbs_id2")
    private String busOrBbsId2;

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((CreateCouplingDeviceInfos) modificationInfos);
    }

    private void assignAttributes(CreateCouplingDeviceInfos createCouplingDeviceInfos) {
        this.voltageLevelId = createCouplingDeviceInfos.getVoltageLevelId();
        this.busOrBbsId1 = createCouplingDeviceInfos.getBusOrBbsId1();
        this.busOrBbsId2 = createCouplingDeviceInfos.getBusOrBbsId2();
    }

    public CreateCouplingDeviceEntity(CreateCouplingDeviceInfos createCouplingDeviceInfos) {
        super(createCouplingDeviceInfos);
        assignAttributes(createCouplingDeviceInfos);
    }

    @Override
    public CreateCouplingDeviceInfos toModificationInfos() {
        return toCreateCouplingDeviceInfos().build();
    }

    private CreateCouplingDeviceInfos.CreateCouplingDeviceInfosBuilder<?, ?> toCreateCouplingDeviceInfos() {
        return CreateCouplingDeviceInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .voltageLevelId(getVoltageLevelId())
            .busOrBbsId1(getBusOrBbsId1())
            .busOrBbsId2(getBusOrBbsId2());
    }
}
