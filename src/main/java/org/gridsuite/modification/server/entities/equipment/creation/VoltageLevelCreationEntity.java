/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "voltageLevelCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelCreation_id_fk_constraint"))
public class VoltageLevelCreationEntity extends EquipmentCreationEntity {

    @Column(name = "nominalVoltage")
    private double nominalVoltage;

    @Column(name = "substationId")
    private String substationId;

    public VoltageLevelCreationEntity(String equipmentId, String equipmentName, double nominalVoltage, String substationId) {
        super(ModificationType.VOLTAGE_LEVEL_CREATION,
                equipmentId,
                equipmentName);
        this.nominalVoltage = nominalVoltage;
        this.substationId = substationId;
    }

    public VoltageLevelCreationInfos toVoltageLevelCreationInfos() {
        return toVoltageLevelCreationInfosBuilder().build();
    }

    private VoltageLevelCreationInfos.VoltageLevelCreationInfosBuilder<?, ?> toVoltageLevelCreationInfosBuilder() {
        return VoltageLevelCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .nominalVoltage(getNominalVoltage())
                .substationId(getSubstationId());
    }
}

