/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.EnergySource;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveModificationInfos;
import org.gridsuite.modification.server.dto.VoltageRegulationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.BooleanModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.EnumModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.FloatModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

import javax.persistence.AttributeOverride;
import javax.persistence.AttributeOverrides;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "generatorModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "generatorModification_id_fk_constraint"))
public class GeneratorModificationEntity extends InjectionModificationEntity {
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "energySource")),
        @AttributeOverride(name = "opType", column = @Column(name = "energySourceOp"))
    })
    EnumModificationEmbedded<EnergySource> energySource;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "minActivePowerOp"))
    })
    DoubleModificationEmbedded minActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxActivePowerOp"))
    })
    DoubleModificationEmbedded maxActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedNominalPower")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedNominalPowerOp"))
    })
    DoubleModificationEmbedded ratedNominalPower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    DoubleModificationEmbedded activePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageSetpointOp"))
    })
    DoubleModificationEmbedded voltageSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "marginalCost")),
        @AttributeOverride(name = "opType", column = @Column(name = "marginalCostOp"))
    })
    DoubleModificationEmbedded marginalCost;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minimumReactivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "minimumReactivePowerOp"))
    })
    DoubleModificationEmbedded minimumReactivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maximumReactivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "maximumReactivePowerOp"))
    })
    DoubleModificationEmbedded maximumReactivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "participate")),
        @AttributeOverride(name = "opType", column = @Column(name = "participateOp"))
    })
    BooleanModificationEmbedded participate;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "droop")),
        @AttributeOverride(name = "opType", column = @Column(name = "droopOp"))
    })
    FloatModificationEmbedded droop;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "transientReactance")),
        @AttributeOverride(name = "opType", column = @Column(name = "transientReactanceOp"))
    })
    DoubleModificationEmbedded transientReactance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "stepUpTransformerReactance")),
        @AttributeOverride(name = "opType", column = @Column(name = "stepUpTransformerReactanceOp"))
    })
    DoubleModificationEmbedded stepUpTransformerReactance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationType")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationTypeOp"))
    })
    EnumModificationEmbedded<VoltageRegulationType> voltageRegulationType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalIdOp"))
    })
    StringModificationEmbedded regulatingTerminalId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalType")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalTypeOp"))
    })
    StringModificationEmbedded regulatingTerminalType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalVlId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalVlIdOp"))
    })
    StringModificationEmbedded regulatingTerminalVlId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "qPercent")),
        @AttributeOverride(name = "opType", column = @Column(name = "qPercentOp"))
    })
    DoubleModificationEmbedded qPercent;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactiveCapabilityCurve")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactiveCapabilityCurveOp"))
    })
    BooleanModificationEmbedded reactiveCapabilityCurve;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints;

    public GeneratorModificationEntity(String equipmentId, AttributeModification<String> equipmentName,
            AttributeModification<String> voltageLevelId, AttributeModification<String> busOrBusbarSectionId,
            AttributeModification<EnergySource> energySource,
            AttributeModification<Double> minActivePower, AttributeModification<Double> maxActivePower,
            AttributeModification<Double> ratedNominalPower,
            AttributeModification<Double> activePowerSetpoint, AttributeModification<Double> reactivePowerSetpoint,
            AttributeModification<Boolean> voltageRegulationOn, AttributeModification<Double> voltageSetpoint,
            AttributeModification<Double> marginalCost, AttributeModification<Double> minimumReactivePower,
            AttributeModification<Double> maximumReactivePower, AttributeModification<Boolean> participate,
            AttributeModification<Float> droop, AttributeModification<Double> transientReactance,
            AttributeModification<Double> stepUpTransformerReactance, AttributeModification<VoltageRegulationType> voltageRegulationType,
            AttributeModification<String> regulatingTerminalId, AttributeModification<String> regulatingTerminalType,
            AttributeModification<String> regulatingTerminalVlId,
            AttributeModification<Double> qPercent,
            AttributeModification<Boolean> reactiveCapabilityCurve,
            List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints) {
        super(ModificationType.GENERATOR_MODIFICATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.energySource = new EnumModificationEmbedded<>(energySource);
        this.minActivePower = new DoubleModificationEmbedded(minActivePower);
        this.maxActivePower = new DoubleModificationEmbedded(maxActivePower);
        this.ratedNominalPower = new DoubleModificationEmbedded(ratedNominalPower);
        this.activePowerSetpoint = new DoubleModificationEmbedded(activePowerSetpoint);
        this.reactivePowerSetpoint = new DoubleModificationEmbedded(reactivePowerSetpoint);
        this.voltageRegulationOn = new BooleanModificationEmbedded(voltageRegulationOn);
        this.voltageSetpoint = new DoubleModificationEmbedded(voltageSetpoint);
        this.marginalCost = new DoubleModificationEmbedded(marginalCost);
        this.minimumReactivePower = new DoubleModificationEmbedded(minimumReactivePower);
        this.maximumReactivePower = new DoubleModificationEmbedded(maximumReactivePower);
        this.participate = new BooleanModificationEmbedded(participate);
        this.droop = new FloatModificationEmbedded(droop);
        this.transientReactance = new DoubleModificationEmbedded(transientReactance);
        this.stepUpTransformerReactance = new DoubleModificationEmbedded(stepUpTransformerReactance);
        this.voltageRegulationType = new EnumModificationEmbedded<>(voltageRegulationType);
        this.regulatingTerminalId = new StringModificationEmbedded(regulatingTerminalId);
        this.regulatingTerminalType = new StringModificationEmbedded(regulatingTerminalType);
        this.regulatingTerminalVlId = new StringModificationEmbedded(regulatingTerminalVlId);
        this.qPercent = new DoubleModificationEmbedded(qPercent);
        this.reactiveCapabilityCurve = new BooleanModificationEmbedded(reactiveCapabilityCurve);
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
    }

    public static List<ReactiveCapabilityCurveModificationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveModificationInfos> points) {
        return points == null ? null
                : points.stream()
                        .map(point -> new ReactiveCapabilityCurveModificationEmbeddable(point.getQminP(), point.getOldQminP(),
                                point.getQmaxP(), point.getOldQmaxP(), point.getP(),
                                point.getOldP()))
                        .collect(Collectors.toList());
    }

    public GeneratorModificationEntity(GeneratorModificationInfos infos) {
        this(infos.getEquipmentId(), infos.getEquipmentName(), infos.getVoltageLevelId(),
                infos.getBusOrBusbarSectionId(),
                infos.getEnergySource(),
                infos.getMinActivePower(), infos.getMaxActivePower(), infos.getRatedNominalPower(),
                infos.getActivePowerSetpoint(), infos.getReactivePowerSetpoint(), infos.getVoltageRegulationOn(),
                infos.getVoltageSetpoint(), infos.getMarginalCost(), infos.getMinimumReactivePower(), infos.getMaximumReactivePower(), infos.getParticipate(),
                infos.getDroop(), infos.getTransientReactance(), infos.getStepUpTransformerReactance(), infos.getVoltageRegulationType(), infos.getRegulatingTerminalId(), infos.getRegulatingTerminalType(),
                infos.getRegulatingTerminalVlId(), infos.getQPercent(), infos.getReactiveCapabilityCurve(), toEmbeddablePoints(infos.getReactiveCapabilityCurvePoints()));
    }

    @Override
    public GeneratorModificationInfos toModificationInfos() {
        return toGeneratorModificationInfosBuilder().build();
    }

    private GeneratorModificationInfos.GeneratorModificationInfosBuilder<?, ?> toGeneratorModificationInfosBuilder() {
        List<ReactiveCapabilityCurveModificationEmbeddable> pointsEmbeddable = getReactiveCapabilityCurvePoints();
        List<ReactiveCapabilityCurveModificationInfos> points = pointsEmbeddable != null ? getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveModificationInfos(value.getQminP(), value.getOldQminP(),
                        value.getQmaxP(), value.getOldQmaxP(),
                        value.getP(), value.getOldP()))
                .collect(Collectors.toList()) : null;
        return GeneratorModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(new AttributeModification<>(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(new AttributeModification<>(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(new AttributeModification<>(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
            .energySource(toAttributeModification(getEnergySource()))
            .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
            .maxActivePower(toAttributeModification(getMaxActivePower()))
            .minActivePower(toAttributeModification(getMinActivePower()))
            .ratedNominalPower(toAttributeModification(getRatedNominalPower()))
            .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
            .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
            .type(ModificationType.GENERATOR_MODIFICATION)
            .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
            .marginalCost(toAttributeModification(getMarginalCost()))
            .minimumReactivePower(toAttributeModification(getMinimumReactivePower()))
            .maximumReactivePower(toAttributeModification(getMaximumReactivePower()))
            .participate(toAttributeModification(getParticipate()))
            .droop(toAttributeModification(getDroop()))
            .transientReactance(toAttributeModification(getTransientReactance()))
            .stepUpTransformerReactance(toAttributeModification(getStepUpTransformerReactance()))
            .voltageRegulationType(toAttributeModification(getVoltageRegulationType()))
            .regulatingTerminalId(toAttributeModification(getRegulatingTerminalId()))
            .regulatingTerminalType(toAttributeModification(getRegulatingTerminalType()))
            .regulatingTerminalVlId(toAttributeModification(getRegulatingTerminalVlId()))
            .qPercent(toAttributeModification(getQPercent()))
            .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
            .reactiveCapabilityCurvePoints(points);
    }

    @Override
    public void cloneWithIdsToNull() {
        super.cloneWithIdsToNull();
        this.reactiveCapabilityCurvePoints = new ArrayList<>(reactiveCapabilityCurvePoints);
    }
}
