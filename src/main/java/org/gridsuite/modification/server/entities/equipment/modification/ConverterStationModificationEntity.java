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
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;

import java.util.List;

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
    @CollectionTable(name = "converter_station_modification_rcc_points")
    private List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactiveCapabilityCurve")), @AttributeOverride(name = "opType", column = @Column(name = "reactiveCapabilityCurveOp"))
    })
    private BooleanModificationEmbedded reactiveCapabilityCurve;

    public ConverterStationModificationEntity(ConverterStationModificationInfos converterStationModificationInfos) {
        super(converterStationModificationInfos);
        assignAttributes(converterStationModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ConverterStationModificationInfos) modificationInfos);
    }

    private void assignAttributes(ConverterStationModificationInfos converterStationModificationInfos) {
        this.lossFactor = converterStationModificationInfos.getLossFactor() != null ? new FloatModificationEmbedded(converterStationModificationInfos.getLossFactor()) : null;
        this.minQ = converterStationModificationInfos.getMinQ() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getMinQ()) : null;
        this.maxQ = converterStationModificationInfos.getMaxQ() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getMaxQ()) : null;
        this.reactivePowerSetpoint = converterStationModificationInfos.getReactivePowerSetpoint() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getReactivePowerSetpoint()) : null;
        this.voltageRegulationOn = converterStationModificationInfos.getVoltageRegulationOn() != null ? new BooleanModificationEmbedded(converterStationModificationInfos.getVoltageRegulationOn()) : null;
        this.voltageSetpoint = converterStationModificationInfos.getVoltageSetpoint() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getVoltageSetpoint()) : null;
        this.reactiveCapabilityCurve = converterStationModificationInfos.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(converterStationModificationInfos.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(converterStationModificationInfos.getReactiveCapabilityCurvePoints());

    }

    public static List<ReactiveCapabilityCurveModificationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveModificationInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveModificationEmbeddable(point.getPointPosition(), point.getMinQ(), point.getOldMinQ(),
                        point.getMaxQ(), point.getOldMaxQ(), point.getP(),
                        point.getOldP()))
                .toList();
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
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .lossFactor(toAttributeModification(getLossFactor()))
            .minQ(toAttributeModification(getMinQ()))
            .maxQ(toAttributeModification(getMaxQ()))
            .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
            .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
            .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
            .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
            .reactiveCapabilityCurvePoints(DTOUtils.convertToReactiveCapabilityCurveModificationInfos(getReactiveCapabilityCurvePoints()));
    }
}
