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
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minimumReactivePower")), @AttributeOverride(name = "opType", column = @Column(name = "minimumReactivePowerOp"))
    })
    private DoubleModificationEmbedded minimumReactivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maximumReactivePower")), @AttributeOverride(name = "opType", column = @Column(name = "maximumReactivePowerOp"))
    })
    private DoubleModificationEmbedded maximumReactivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactivePower")), @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerOp"))
    })
    private DoubleModificationEmbedded reactivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")), @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    private BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "voltage")), @AttributeOverride(name = "opType", column = @Column(name = "voltageOp"))
    })
    private DoubleModificationEmbedded voltage;

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
        this.minimumReactivePower = converterStationModificationInfos.getMinimumReactivePower() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getMinimumReactivePower()) : null;
        this.maximumReactivePower = converterStationModificationInfos.getMaximumReactivePower() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getMaximumReactivePower()) : null;
        this.reactivePower = converterStationModificationInfos.getReactivePower() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getReactivePower()) : null;
        this.voltageRegulationOn = converterStationModificationInfos.getVoltageRegulationOn() != null ? new BooleanModificationEmbedded(converterStationModificationInfos.getVoltageRegulationOn()) : null;
        this.voltage = converterStationModificationInfos.getVoltage() != null ? new DoubleModificationEmbedded(converterStationModificationInfos.getVoltage()) : null;
        this.reactiveCapabilityCurve = converterStationModificationInfos.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(converterStationModificationInfos.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(converterStationModificationInfos.getReactiveCapabilityCurvePoints());

    }

    public static List<ReactiveCapabilityCurveModificationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveModificationInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveModificationEmbeddable(point.getQminP(), point.getOldQminP(),
                        point.getQmaxP(), point.getOldQmaxP(), point.getP(),
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
            .equipmentId(getEquipmentId())
            .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
            .lossFactor(toAttributeModification(getLossFactor()))
            .minimumReactivePower(toAttributeModification(getMinimumReactivePower()))
            .maximumReactivePower(toAttributeModification(getMaximumReactivePower()))
            .reactivePower(toAttributeModification(getReactivePower()))
            .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
            .voltage(toAttributeModification(getVoltage()))
            .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
            .reactiveCapabilityCurvePoints(DTOUtils.convertToReactiveCapabilityCurveModificationInfos(getReactiveCapabilityCurvePoints()));
    }
}
