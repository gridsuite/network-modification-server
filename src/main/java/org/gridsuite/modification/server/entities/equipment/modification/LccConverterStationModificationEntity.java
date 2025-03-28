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
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.LccConverterStationModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;

import java.util.List;

import static org.gridsuite.modification.server.entities.equipment.modification.ShuntCompensatorModificationEmbeddable.fromEmbeddableShuntCompensatorModification;
import static org.gridsuite.modification.server.entities.equipment.modification.ShuntCompensatorModificationEmbeddable.toEmbeddableShuntCompensatorModification;
import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "lccConverterStationModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "lcc_converter_station_modification_id_fk_constraint"))
public class LccConverterStationModificationEntity extends InjectionModificationEntity {

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "lossFactor")), @AttributeOverride(name = "opType", column = @Column(name = "lossFactorOp"))
    })
    private FloatModificationEmbedded lossFactor;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "powerFactor")), @AttributeOverride(name = "opType", column = @Column(name = "powerFactorOp"))
    })
    private FloatModificationEmbedded powerFactor;

    @ElementCollection
    @CollectionTable(name = "shunt_compensator_on_side")
    private List<ShuntCompensatorModificationEmbeddable> shuntCompensatorsOnSide;

    public LccConverterStationModificationEntity(LccConverterStationModificationInfos converterStationModificationInfos) {
        super(converterStationModificationInfos);
        assignAttributes(converterStationModificationInfos);
    }

    private void assignAttributes(LccConverterStationModificationInfos converterStationModificationInfos) {
        this.lossFactor = converterStationModificationInfos.getLossFactor() != null ? new FloatModificationEmbedded(converterStationModificationInfos.getLossFactor()) : null;
        this.powerFactor = converterStationModificationInfos.getPowerFactor() != null ? new FloatModificationEmbedded(converterStationModificationInfos.getPowerFactor()) : null;
        this.shuntCompensatorsOnSide = toEmbeddableShuntCompensatorModification(converterStationModificationInfos.getShuntCompensatorsOnSide());
    }

    private LccConverterStationModificationInfos.LccConverterStationModificationInfosBuilder<?, ?> toConverterStationModificationInfoBuilder() {

        return LccConverterStationModificationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .lossFactor(toAttributeModification(getLossFactor()))
            .powerFactor(toAttributeModification(getPowerFactor()))
            .shuntCompensatorsOnSide(fromEmbeddableShuntCompensatorModification(getShuntCompensatorsOnSide()));
    }
}
