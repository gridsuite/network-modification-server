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

import javax.persistence.*;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lineCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lineCreation_id_fk_constraint"))
public class LineCreationEntity extends BranchCreationEntity {

    @Column(name = "shuntConductance1")
    private Double shuntConductance1;

    @Column(name = "shuntSusceptance1")
    private Double shuntSusceptance1;

    @Column(name = "shuntConductance2")
    private Double shuntConductance2;

    @Column(name = "shuntSusceptance2")
    private Double shuntSusceptance2;

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
        shuntConductance1 = lineCreationInfos.getShuntConductance1();
        shuntSusceptance1 = lineCreationInfos.getShuntSusceptance1();
        shuntConductance2 = lineCreationInfos.getShuntConductance2();
        shuntSusceptance2 = lineCreationInfos.getShuntSusceptance2();
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
            .equipmentId(getEquipmentId())
            .name(getEquipmentName())
            .seriesResistance(getSeriesResistance())
            .seriesReactance(getSeriesReactance())
            .shuntConductance1(getShuntConductance1())
            .shuntSusceptance1(getShuntSusceptance1())
            .shuntConductance2(getShuntConductance2())
            .shuntSusceptance2(getShuntSusceptance2())
            .voltageLevelId1(getVoltageLevelId1())
            .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
            .voltageLevelId2(getVoltageLevelId2())
            .busOrBusbarSectionId2(getBusOrBusbarSectionId2())
            .connectionName1(getConnectionName1())
            .connectionDirection1(getConnectionDirection1())
            .connectionName2(getConnectionName2())
            .connectionDirection2(getConnectionDirection2())
            .connectionPosition1(getConnectionPosition1())
            .connectionPosition2(getConnectionPosition2());

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }
        return builder;
    }

}
