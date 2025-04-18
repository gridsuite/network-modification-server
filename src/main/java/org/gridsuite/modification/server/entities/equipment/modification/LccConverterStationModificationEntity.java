/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.AttributeOverride;
import jakarta.persistence.AttributeOverrides;
import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;

import java.util.List;

import static org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEmbeddable.fromEmbeddableShuntCompensatorCreation;
import static org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEmbeddable.toEmbeddableShuntCompensatorCreation;
import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lcc_converter_station_modification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lcc_converter_station_modification_id_fk_constraint"))
public class LccConverterStationModificationEntity extends InjectionModificationEntity {

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "loss_factor")), @AttributeOverride(name = "opType", column = @Column(name = "lossFactorOp"))
    })
    private FloatModificationEmbedded lossFactor;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "power_factor")), @AttributeOverride(name = "opType", column = @Column(name = "powerFactorOp"))
    })
    private FloatModificationEmbedded powerFactor;

    @ElementCollection
    @CollectionTable(name = "lcc_converter_station_modification_on_side", joinColumns = @JoinColumn(name = "lcc_converter_station_modification_id"),
        foreignKey = @ForeignKey(name = "lcc_converter_station_modification_on_side_fk"))
    private List<ShuntCompensatorCreationEmbeddable> shuntCompensatorsOnSide;

    public LccConverterStationModificationEntity(LccConverterStationModificationInfos converterStationModificationInfos) {
        super(converterStationModificationInfos);
        assignAttributes(converterStationModificationInfos);
    }

    private void assignAttributes(LccConverterStationModificationInfos converterStationModificationInfos) {
        this.lossFactor = converterStationModificationInfos.getLossFactor() != null ? new FloatModificationEmbedded(converterStationModificationInfos.getLossFactor()) : null;
        this.powerFactor = converterStationModificationInfos.getPowerFactor() != null ? new FloatModificationEmbedded(converterStationModificationInfos.getPowerFactor()) : null;
        this.shuntCompensatorsOnSide = toEmbeddableShuntCompensatorCreation(converterStationModificationInfos.getShuntCompensatorsOnSide());
    }

    public LccConverterStationModificationInfos toLccConverterStationInfos() {
        return LccConverterStationModificationInfos.builder()
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
            .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
            .connectionName(toAttributeModification(getConnectionName()))
            .connectionPosition(toAttributeModification(getConnectionPosition()))
            .connectionDirection(toAttributeModification(getConnectionDirection()))
            .terminalConnected(toAttributeModification(getTerminalConnected()))
            // ConverterStation
            .lossFactor(toAttributeModification(getLossFactor()))
            .powerFactor(toAttributeModification(getPowerFactor()))
            .shuntCompensatorsOnSide(fromEmbeddableShuntCompensatorCreation(getShuntCompensatorsOnSide()))
            .build();
    }
}
