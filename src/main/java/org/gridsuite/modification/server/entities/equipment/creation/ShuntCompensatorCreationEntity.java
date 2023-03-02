/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;

import javax.persistence.*;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "shuntCompensatorCreation_id_fk_constraint"))
public class ShuntCompensatorCreationEntity extends InjectionCreationEntity {

    @Column
    int maximumNumberOfSections;

    @Column
    int currentNumberOfSections;

    @Column
    Double susceptancePerSection;

    @Column
    Double qAtNominalV;

    @Column
    private ShuntCompensatorType shuntCompensatorType;

    @Column
    boolean isIdenticalSections;

    @Column
    private String connectionName;

    @Column
    private ConnectablePosition.Direction connectionDirection;

    @Column(name = "connectionPosition")
    private Integer connectionPosition;

    public ShuntCompensatorCreationEntity(ShuntCompensatorCreationInfos creationInfos) {
        super(creationInfos);
        maximumNumberOfSections = creationInfos.getMaximumNumberOfSections();
        currentNumberOfSections = creationInfos.getCurrentNumberOfSections();
        susceptancePerSection = creationInfos.getSusceptancePerSection();
        qAtNominalV = creationInfos.getQAtNominalV();
        shuntCompensatorType = creationInfos.getShuntCompensatorType();
        isIdenticalSections = creationInfos.getIsIdenticalSection();
        connectionName = creationInfos.getConnectionName();
        connectionDirection = creationInfos.getConnectionDirection();
        connectionPosition = creationInfos.getConnectionPosition();
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        ShuntCompensatorCreationInfos shuntCompensatorCreationInfos = (ShuntCompensatorCreationInfos) modificationInfos;
        maximumNumberOfSections = shuntCompensatorCreationInfos.getMaximumNumberOfSections();
        currentNumberOfSections = shuntCompensatorCreationInfos.getCurrentNumberOfSections();
        susceptancePerSection = shuntCompensatorCreationInfos.getSusceptancePerSection();
        qAtNominalV = shuntCompensatorCreationInfos.getQAtNominalV();
        shuntCompensatorType = shuntCompensatorCreationInfos.getShuntCompensatorType();
        isIdenticalSections = shuntCompensatorCreationInfos.getIsIdenticalSection();
        connectionName = shuntCompensatorCreationInfos.getConnectionName();
        connectionDirection = shuntCompensatorCreationInfos.getConnectionDirection();
        connectionPosition = shuntCompensatorCreationInfos.getConnectionPosition();
    }

    @Override
    public ShuntCompensatorCreationInfos toModificationInfos() {
        return toShuntCompensatorCreationInfosBuilder().build();
    }

    private ShuntCompensatorCreationInfos.ShuntCompensatorCreationInfosBuilder<?, ?> toShuntCompensatorCreationInfosBuilder() {
        return ShuntCompensatorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .isIdenticalSection(isIdenticalSections())
            .currentNumberOfSections(getCurrentNumberOfSections())
            .maximumNumberOfSections(getMaximumNumberOfSections())
            .susceptancePerSection(getSusceptancePerSection())
            .qAtNominalV(getQAtNominalV())
            .shuntCompensatorType(getShuntCompensatorType())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection())
            .connectionPosition(getConnectionPosition());
    }

}
