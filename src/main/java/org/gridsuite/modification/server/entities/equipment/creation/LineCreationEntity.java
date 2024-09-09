/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.LineCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import jakarta.persistence.*;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lineCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lineCreation_id_fk_constraint"))
public class LineCreationEntity extends BranchCreationEntity {

    @Column(name = "g1")
    private Double g1;

    @Column(name = "b1")
    private Double b1;

    @Column(name = "g2")
    private Double g2;

    @Column(name = "b2")
    private Double b2;

    public LineCreationEntity(LineCreationInfos lineCreationInfos) {
        super(lineCreationInfos);
        assignAttributes(lineCreationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        LineCreationInfos lineCreationInfos = (LineCreationInfos) modificationInfos;
        assignAttributes(lineCreationInfos);
    }

    private void assignAttributes(LineCreationInfos lineCreationInfos) {
        g1 = lineCreationInfos.getG1();
        b1 = lineCreationInfos.getB1();
        g2 = lineCreationInfos.getG2();
        b2 = lineCreationInfos.getB2();
    }

    @Override
    public LineCreationInfos toModificationInfos() {
        return toLineCreationInfosBuilder().build();
    }

    private LineCreationInfos.LineCreationInfosBuilder<?, ?> toLineCreationInfosBuilder() {
        LineCreationInfos.LineCreationInfosBuilder<?, ?> builder = LineCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .active(getActive())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            // branch
            .r(getR())
            .x(getX())
            .voltageLevelId1(getVoltageLevelId1())
            .voltageLevelId2(getVoltageLevelId2())
            .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
            .busOrBusbarSectionId2(getBusOrBusbarSectionId2())
            .connectionName1(getConnectionName1())
            .connectionName2(getConnectionName2())
            .connectionDirection1(getConnectionDirection1())
            .connectionDirection2(getConnectionDirection2())
            .connectionPosition1(getConnectionPosition1())
            .connectionPosition2(getConnectionPosition2())
            .connected1(isConnected1())
            .connected2(isConnected2())
            // line
            .g1(getG1())
            .b1(getB1())
            .g2(getG2())
            .b2(getB2())
             // properties
             .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }
        return builder;
    }

}
