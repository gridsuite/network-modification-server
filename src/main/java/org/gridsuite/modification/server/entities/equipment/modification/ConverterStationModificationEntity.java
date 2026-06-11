/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.model.AttributeModification;
import org.gridsuite.modification.model.ConverterStationModificationModel;
import org.gridsuite.modification.server.dto.DTOUtils;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;

import java.util.List;

import static org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable.toEmbeddablePoints;
import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "converterStationModification")
public class ConverterStationModificationEntity extends InjectionModificationEntity {

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "lossFactor")), @AttributeOverride(name = "opType", column = @Column(name = "lossFactorOp"))
    })
    private FloatModificationEmbedded lossFactor;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minQ")), @AttributeOverride(name = "opType", column = @Column(name = "minqOp"))
    })
    private DoubleModificationEmbedded minQ;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maxQ")), @AttributeOverride(name = "opType", column = @Column(name = "maxqOp"))
    })
    private DoubleModificationEmbedded maxQ;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")), @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    private DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")), @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    private BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "voltageSetpoint")), @AttributeOverride(name = "opType", column = @Column(name = "voltageSetpointOp"))
    })
    private DoubleModificationEmbedded voltageSetpoint;

    @ElementCollection
    @CollectionTable(name = "converter_station_modification_rcc_points", indexes = @Index(name = "converter_station_modification_rcc_points_entity_id_idx", columnList = "converter_station_modification_entity_id"))
    private List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactiveCapabilityCurve")), @AttributeOverride(name = "opType", column = @Column(name = "reactiveCapabilityCurveOp"))
    })
    private BooleanModificationEmbedded reactiveCapabilityCurve;

    public ConverterStationModificationEntity(ModificationInfos converterStationModificationInfos) {
        super(converterStationModificationInfos);
        assignAttributes((ConverterStationModificationModel) converterStationModificationInfos.toModel());
    }

    public ConverterStationModificationEntity(ConverterStationModificationModel converterStationModificationModel) {
        super(converterStationModificationModel);
        assignAttributes(converterStationModificationModel);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ConverterStationModificationModel) modificationInfos.toModel());
    }

    private void assignAttributes(ConverterStationModificationModel converterStationModificationModel) {
        this.lossFactor = converterStationModificationModel.getLossFactor() != null ? new FloatModificationEmbedded(converterStationModificationModel.getLossFactor()) : null;
        this.minQ = converterStationModificationModel.getMinQ() != null ? new DoubleModificationEmbedded(converterStationModificationModel.getMinQ()) : null;
        this.maxQ = converterStationModificationModel.getMaxQ() != null ? new DoubleModificationEmbedded(converterStationModificationModel.getMaxQ()) : null;
        this.reactivePowerSetpoint = converterStationModificationModel.getReactivePowerSetpoint() != null ? new DoubleModificationEmbedded(converterStationModificationModel.getReactivePowerSetpoint()) : null;
        this.voltageRegulationOn = converterStationModificationModel.getVoltageRegulationOn() != null ? new BooleanModificationEmbedded(converterStationModificationModel.getVoltageRegulationOn()) : null;
        this.voltageSetpoint = converterStationModificationModel.getVoltageSetpoint() != null ? new DoubleModificationEmbedded(converterStationModificationModel.getVoltageSetpoint()) : null;
        this.reactiveCapabilityCurve = converterStationModificationModel.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(converterStationModificationModel.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(converterStationModificationModel.getReactiveCapabilityCurvePoints());

    }

    @Override
    public ConverterStationModificationInfos toModificationInfos() {
        return toConverterStationModificationInfoBuilder().build();
    }

    private ConverterStationModificationInfos.ConverterStationModificationInfosBuilder<?, ?> toConverterStationModificationInfoBuilder() {

        return ConverterStationModificationInfos.builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .activated(getActivated())
            .description(getDescription())
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .lossFactor(toAttributeModification(getLossFactor()))
            .minQ(toAttributeModification(getMinQ()))
            .maxQ(toAttributeModification(getMaxQ()))
            .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
            .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
            .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
            .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
            .reactiveCapabilityCurvePoints(DTOUtils.toReactiveCapabilityCurvePointsModificationModel(getReactiveCapabilityCurvePoints()));
    }
}
