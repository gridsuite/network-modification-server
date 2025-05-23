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
import org.gridsuite.modification.dto.AddCouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

/**
 * @author etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "addCouplingDevice")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "addCouplingDevice_id_fk_constraint"))
public class AddCouplingDeviceEntity extends ModificationEntity {
    @Column
    private String voltageLevelId;
    @Column
    private String busOrBbsId1;
    @Column
    private String busOrBbsId2;

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((AddCouplingDeviceInfos) modificationInfos);
    }

    private void assignAttributes(AddCouplingDeviceInfos addCouplingDeviceInfos) {
        this.voltageLevelId = addCouplingDeviceInfos.getVoltageLevelId();
        this.busOrBbsId1 = addCouplingDeviceInfos.getBusOrBbsId1();
        this.busOrBbsId2 = addCouplingDeviceInfos.getBusOrBbsId2();
    }

    public AddCouplingDeviceEntity(AddCouplingDeviceInfos addCouplingDeviceInfos) {
        super(addCouplingDeviceInfos);
        assignAttributes(addCouplingDeviceInfos);
    }

    @Override
    public AddCouplingDeviceInfos toModificationInfos() {
        return toAddCouplingDeviceInfos().build();
    }

    private AddCouplingDeviceInfos.AddCouplingDeviceInfosBuilder<?, ?> toAddCouplingDeviceInfos() {
        return AddCouplingDeviceInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .voltageLevelId(getVoltageLevelId())
            .busOrBbsId1(getBusOrBbsId1())
            .busOrBbsId2(getBusOrBbsId2());
    }
}
