/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.CreateVoltageLevelSectionInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

/**
 * @author Rehili Ghazwa <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "createVoltageLevelSection_id_fk_constraint"))
public class CreateVoltageLevelSectionEntity extends ModificationEntity {

    @Column
    private String voltageLevelId;

    @Column
    private String busbarSectionId;

    @Column
    private boolean isAfterBusbarSectionId;

    @Column
    private int busbarCount;

    @Column
    private int sectionCount;

    @Column
    private String leftSwitchKind;

    @Column
    private String rightSwitchKind;

    @Column
    private boolean isAllBusbars;

    public CreateVoltageLevelSectionEntity(CreateVoltageLevelSectionInfos createVoltageLevelSectionInfos) {
        super(createVoltageLevelSectionInfos);
        assignAttributes(createVoltageLevelSectionInfos);
    }

    @Override
    public CreateVoltageLevelSectionInfos toModificationInfos() {
        return toCreateVoltageLevelSectionInfosBuilder().build();
    }

    public CreateVoltageLevelSectionInfos toCreateVoltageLevelSectionInfos() {
        return toCreateVoltageLevelSectionInfosBuilder()
                .build();
    }

    private CreateVoltageLevelSectionInfos.CreateVoltageLevelSectionInfosBuilder<?, ?> toCreateVoltageLevelSectionInfosBuilder() {
        return CreateVoltageLevelSectionInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .voltageLevelId(getVoltageLevelId())
                .busbarSectionId(getBusbarSectionId())
                .busbarCount(getBusbarCount())
                .sectionCount(getSectionCount())
                .isAllBusbars(isAllBusbars())
                .rightSwitchKind(getRightSwitchKind())
                .leftSwitchKind(getLeftSwitchKind())
                .isAfterBusbarSectionId(isAfterBusbarSectionId());
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((CreateVoltageLevelSectionInfos) modificationInfos);
    }

    private void assignAttributes(CreateVoltageLevelSectionInfos createVoltageLevelSectionInfos) {
        this.voltageLevelId = createVoltageLevelSectionInfos.getVoltageLevelId();
        this.busbarSectionId = createVoltageLevelSectionInfos.getBusbarSectionId();
        this.busbarCount = createVoltageLevelSectionInfos.getBusbarCount();
        this.sectionCount = createVoltageLevelSectionInfos.getSectionCount();
        this.isAllBusbars = createVoltageLevelSectionInfos.isAllBusbars();
        this.leftSwitchKind = createVoltageLevelSectionInfos.getLeftSwitchKind();
        this.rightSwitchKind = createVoltageLevelSectionInfos.getRightSwitchKind();
        this.isAfterBusbarSectionId = createVoltageLevelSectionInfos.isAfterBusbarSectionId();
    }
}

