/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LineCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

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

    public LineCreationEntity(String equipmentId,
                                String equipmentName,
                                double seriesResistance,
                                double seriesReactance,
                                Double shuntConductance1,
                                Double shuntSusceptance1,
                                Double shuntConductance2,
                                Double shuntSusceptance2,
                                String voltageLevelId1,
                                String busOrBusbarSectionId1,
                                String voltageLevelId2,
                                String busOrBusbarSectionId2,
                                Double permanentCurrentLimit1,
                                Double permanentCurrentLimit2
    ) {
        super(ModificationType.LINE_CREATION,
                equipmentId,
                equipmentName,
                seriesResistance,
                seriesReactance,
                voltageLevelId1,
                voltageLevelId2,
                busOrBusbarSectionId1,
                busOrBusbarSectionId2,
                permanentCurrentLimit1 != null ? new CurrentLimitsEntity(null, permanentCurrentLimit1) : null,
                permanentCurrentLimit2 != null ? new CurrentLimitsEntity(null, permanentCurrentLimit2) : null
        );
        this.shuntConductance1 = shuntConductance1;
        this.shuntSusceptance1 = shuntSusceptance1;
        this.shuntConductance2 = shuntConductance2;
        this.shuntSusceptance2 = shuntSusceptance2;
    }

    public static LineCreationEntity toEntity(LineCreationInfos lineCreationInfos) {
        return new LineCreationEntity(
                lineCreationInfos.getEquipmentId(), lineCreationInfos.getEquipmentName(),
                lineCreationInfos.getSeriesResistance(), lineCreationInfos.getSeriesReactance(),
                lineCreationInfos.getShuntConductance1(), lineCreationInfos.getShuntSusceptance1(),
                lineCreationInfos.getShuntConductance2(), lineCreationInfos.getShuntSusceptance2(),
                lineCreationInfos.getVoltageLevelId1(), lineCreationInfos.getBusOrBusbarSectionId1(),
                lineCreationInfos.getVoltageLevelId2(), lineCreationInfos.getBusOrBusbarSectionId2(),
                lineCreationInfos.getCurrentLimits1() != null ? lineCreationInfos.getCurrentLimits1().getPermanentLimit() : null,
                lineCreationInfos.getCurrentLimits2() != null ? lineCreationInfos.getCurrentLimits2().getPermanentLimit() : null
        );
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
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .seriesResistance(getSeriesResistance())
            .seriesReactance(getSeriesReactance())
            .shuntConductance1(getShuntConductance1())
            .shuntSusceptance1(getShuntSusceptance1())
            .shuntConductance2(getShuntConductance2())
            .shuntSusceptance2(getShuntSusceptance2())
            .voltageLevelId1(getVoltageLevelId1())
            .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
            .voltageLevelId2(getVoltageLevelId2())
            .busOrBusbarSectionId2(getBusOrBusbarSectionId2());

        if (getCurrentLimits1() != null) {
            builder.currentLimits1(getCurrentLimits1().toCurrentLimitsInfos());
        }
        if (getCurrentLimits2() != null) {
            builder.currentLimits2(getCurrentLimits2().toCurrentLimitsInfos());
        }
        return builder;
    }
}
