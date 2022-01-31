/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "shuntCompensatorCreation_id_fk_constraint"))
public class ShuntCompensatorCreationEntity extends InjectionCreationEntity {

    public ShuntCompensatorCreationEntity(ShuntCompensatorCreationInfos creationInfos) {
        super(ModificationType.SHUNT_COMPENSATOR_CREATION,
            creationInfos.getEquipmentId(), creationInfos.getEquipmentName(),
            creationInfos.getVoltageLevelId(), creationInfos.getBusOrBusbarSectionId());

        maximumNumberOfSections = creationInfos.getMaximumNumberOfSections();
        currentNumberOfSections = creationInfos.getCurrentNumberOfSections();
        susceptancePerSection = creationInfos.getSusceptancePerSection();
        isIdenticalSections = creationInfos.getIsIdenticalSection();
    }

    @Column
    int maximumNumberOfSections;

    @Column
    int currentNumberOfSections;

    @Column
    double susceptancePerSection;

    @Column
    boolean isIdenticalSections;

    @Override
    public ShuntCompensatorCreationInfos toModificationInfos() {
        return toShuntCompensatorCreationInfosBuilder().build();
    }

    private ShuntCompensatorCreationInfos.ShuntCompensatorCreationInfosBuilder<?, ?> toShuntCompensatorCreationInfosBuilder() {
        return ShuntCompensatorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .isIdenticalSection(isIdenticalSections())
            .currentNumberOfSections(getCurrentNumberOfSections())
            .maximumNumberOfSections(getMaximumNumberOfSections())
            .susceptancePerSection(getSusceptancePerSection());
    }

}
