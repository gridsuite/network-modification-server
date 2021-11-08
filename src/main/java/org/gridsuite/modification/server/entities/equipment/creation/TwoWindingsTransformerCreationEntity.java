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
import org.gridsuite.modification.server.dto.TwoWindingsTransformerCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;
import java.util.Set;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "twoWindingsTransformerCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "twoWindingsTransformerCreation_id_fk_constraint"))
public class TwoWindingsTransformerCreationEntity extends BranchCreationEntity {

    @Column(name = "seriesResistance")
    private double seriesResistance;

    @Column(name = "seriesReactance")
    private double seriesReactance;

    @Column(name = "magnetizingConductance")
    private double magnetizingConductance;

    @Column(name = "magnetizingSusceptance")
    private double magnetizingSusceptance;

    @Column(name = "ratedVoltage1")
    private double ratedVoltage1;

    @Column(name = "ratedVoltage2")
    private double ratedVoltage2;

    public TwoWindingsTransformerCreationEntity(String equipmentId,
                              String equipmentName,
                              double seriesResistance,
                              double seriesReactance,
                              double magnetizingConductance,
                              double magnetizingSusceptance,
                              double ratedVoltage1,
                              double ratedVoltage2,
                              String voltageLevelId1,
                              String busOrBusbarSectionId1,
                              String voltageLevelId2,
                              String busOrBusbarSectionId2
    ) {
        super(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, equipmentId, equipmentName, voltageLevelId1, voltageLevelId2, busOrBusbarSectionId1, busOrBusbarSectionId2);
        this.seriesResistance = seriesResistance;
        this.seriesReactance = seriesReactance;
        this.magnetizingConductance = magnetizingConductance;
        this.magnetizingSusceptance = magnetizingSusceptance;
        this.ratedVoltage1 = ratedVoltage1;
        this.ratedVoltage2 = ratedVoltage2;
    }

    public TwoWindingsTransformerCreationInfos toTwoWindingsTransformerCreationInfos() {
        return toTwoWindingsTransformerCreationInfosBuilder().build();
    }

    @Override
    public EquipmenModificationInfos toEquipmentModificationInfos(Set<String> uuids) {
        return toTwoWindingsTransformerCreationInfosBuilder().substationIds(uuids).build();
    }

    private TwoWindingsTransformerCreationInfos.TwoWindingsTransformerCreationInfosBuilder<?, ?> toTwoWindingsTransformerCreationInfosBuilder() {
        return TwoWindingsTransformerCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .seriesResistance(getSeriesResistance())
                .seriesReactance(getSeriesReactance())
                .magnetizingConductance(getMagnetizingConductance())
                .magnetizingSusceptance(getMagnetizingSusceptance())
                .ratedVoltage1(getRatedVoltage1())
                .ratedVoltage2(getRatedVoltage2())
                .voltageLevelId1(getVoltageLevelId1())
                .busOrBusbarSectionId1(getBusOrBusbarSectionId1())
                .voltageLevelId2(getVoltageLevelId2())
                .busOrBusbarSectionId2(getBusOrBusbarSectionId2());
    }
}
