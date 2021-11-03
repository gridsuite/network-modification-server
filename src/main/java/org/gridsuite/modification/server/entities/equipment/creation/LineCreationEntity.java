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
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.LineCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.util.Set;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lineCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lineCreation_id_fk_constraint"))
public class LineCreationEntity extends BranchCreationEntity {

    @Column(name = "seriesResistance")
    private double seriesResistance;

    @Column(name = "seriesReactance")
    private double seriesReactance;

    @Column(name = "shuntConductance1")
    private Double shuntConductance1;

    @Column(name = "shuntSusceptance1")
    private Double shuntSusceptance1;

    @Column(name = "permanentCurrentLimit1")
    private Double permanentCurrentLimit1;

    @Column(name = "shuntConductance2")
    private Double shuntConductance2;

    @Column(name = "shuntSusceptance2")
    private Double shuntSusceptance2;

    @Column(name = "permanentCurrentLimit2")
    private Double permanentCurrentLimit2;

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
        super(ModificationType.LINE_CREATION, equipmentId, equipmentName, voltageLevelId1, voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2);
        this.seriesResistance = seriesResistance;
        this.seriesReactance = seriesReactance;
        this.shuntConductance1 = shuntConductance1;
        this.shuntSusceptance1 = shuntSusceptance1;
        this.shuntConductance2 = shuntConductance2;
        this.shuntSusceptance2 = shuntSusceptance2;
        this.permanentCurrentLimit1 = permanentCurrentLimit1;
        this.permanentCurrentLimit2 = permanentCurrentLimit2;
    }

    public LineCreationInfos toLineCreationInfos() {
        return toLineCreationInfosBuilder().build();
    }

    @Override
    public EquipmenModificationInfos toEquipmentModificationInfos(Set<String> uuids) {
        return toLineCreationInfosBuilder().substationIds(uuids).build();
    }

    private LineCreationInfos.LineCreationInfosBuilder<?, ?> toLineCreationInfosBuilder() {
        return LineCreationInfos
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
            .busOrBusbarSectionId2(getBusOrBusbarSectionId2())
            .permanentCurrentLimit1(getPermanentCurrentLimit1())
            .permanentCurrentLimit2(getPermanentCurrentLimit2());
    }
}
