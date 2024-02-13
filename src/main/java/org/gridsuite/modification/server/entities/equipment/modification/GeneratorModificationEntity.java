/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.EnergySource;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;


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
    private EnumModificationEmbedded<EnergySource> energySource;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "minActivePowerOp"))
    })
    private DoubleModificationEmbedded minActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxActivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxActivePowerOp"))
    })
    private DoubleModificationEmbedded maxActivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedNominalPower")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedNominalPowerOp"))
    })
    private DoubleModificationEmbedded ratedNominalPower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    private DoubleModificationEmbedded activePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    private DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    private BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageSetpoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageSetpointOp"))
    })
    private DoubleModificationEmbedded voltageSetpoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "plannedActivePowerSetPoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "plannedActivePowerSetPointOp"))
    })
    private DoubleModificationEmbedded plannedActivePowerSetPoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "marginalCost")),
        @AttributeOverride(name = "opType", column = @Column(name = "marginalCostOp"))
    })
    private DoubleModificationEmbedded marginalCost;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "plannedOutageRate")),
        @AttributeOverride(name = "opType", column = @Column(name = "plannedOutageRateOp"))
    })
    private DoubleModificationEmbedded plannedOutageRate;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "forcedOutageRate")),
        @AttributeOverride(name = "opType", column = @Column(name = "forcedOutageRateOp"))
    })
    private DoubleModificationEmbedded forcedOutageRate;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "minimumReactivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "minimumReactivePowerOp"))
    })
    private DoubleModificationEmbedded minimumReactivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maximumReactivePower")),
        @AttributeOverride(name = "opType", column = @Column(name = "maximumReactivePowerOp"))
    })
    private DoubleModificationEmbedded maximumReactivePower;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "participate")),
        @AttributeOverride(name = "opType", column = @Column(name = "participateOp"))
    })
    private BooleanModificationEmbedded participate;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "droop")),
        @AttributeOverride(name = "opType", column = @Column(name = "droopOp"))
    })
    private FloatModificationEmbedded droop;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "transientReactance")),
        @AttributeOverride(name = "opType", column = @Column(name = "transientReactanceOp"))
    })
    private DoubleModificationEmbedded transientReactance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "stepUpTransformerReactance")),
        @AttributeOverride(name = "opType", column = @Column(name = "stepUpTransformerReactanceOp"))
    })
    private DoubleModificationEmbedded stepUpTransformerReactance;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationType")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationTypeOp"))
    })
    private EnumModificationEmbedded<VoltageRegulationType> voltageRegulationType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalType")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalTypeOp"))
    })
    private StringModificationEmbedded regulatingTerminalType;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "regulatingTerminalVlId")),
        @AttributeOverride(name = "opType", column = @Column(name = "regulatingTerminalVlIdOp"))
    })
    private StringModificationEmbedded regulatingTerminalVlId;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "qPercent")),
        @AttributeOverride(name = "opType", column = @Column(name = "qPercentOp"))
    })
    private DoubleModificationEmbedded qPercent;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "reactiveCapabilityCurve")),
        @AttributeOverride(name = "opType", column = @Column(name = "reactiveCapabilityCurveOp"))
    })
    private BooleanModificationEmbedded reactiveCapabilityCurve;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints;

    public GeneratorModificationEntity(@NonNull GeneratorModificationInfos generatorModificationInfos) {
        super(generatorModificationInfos);
        assignAttributes(generatorModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GeneratorModificationInfos) modificationInfos);
    }

    private void assignAttributes(GeneratorModificationInfos generatorModificationInfos) {
        this.energySource = generatorModificationInfos.getEnergySource() != null ? new EnumModificationEmbedded<>(generatorModificationInfos.getEnergySource()) : null;
        this.minActivePower = generatorModificationInfos.getMinActivePower() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMinActivePower()) : null;
        this.maxActivePower = generatorModificationInfos.getMaxActivePower() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMaxActivePower()) : null;
        this.ratedNominalPower = generatorModificationInfos.getRatedNominalPower() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getRatedNominalPower()) : null;
        this.activePowerSetpoint = generatorModificationInfos.getActivePowerSetpoint() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getActivePowerSetpoint()) : null;
        this.reactivePowerSetpoint = generatorModificationInfos.getReactivePowerSetpoint() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getReactivePowerSetpoint()) : null;
        this.voltageRegulationOn = generatorModificationInfos.getVoltageRegulationOn() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getVoltageRegulationOn()) : null;
        this.voltageSetpoint = generatorModificationInfos.getVoltageSetpoint() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getVoltageSetpoint()) : null;
        this.plannedActivePowerSetPoint = generatorModificationInfos.getPlannedActivePowerSetPoint() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getPlannedActivePowerSetPoint()) : null;
        this.marginalCost = generatorModificationInfos.getMarginalCost() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMarginalCost()) : null;
        this.plannedOutageRate = generatorModificationInfos.getPlannedOutageRate() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getPlannedOutageRate()) : null;
        this.forcedOutageRate = generatorModificationInfos.getForcedOutageRate() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getForcedOutageRate()) : null;
        this.minimumReactivePower = generatorModificationInfos.getMinimumReactivePower() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMinimumReactivePower()) : null;
        this.maximumReactivePower = generatorModificationInfos.getMaximumReactivePower() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMaximumReactivePower()) : null;
        this.participate = generatorModificationInfos.getParticipate() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getParticipate()) : null;
        this.droop = generatorModificationInfos.getDroop() != null ? new FloatModificationEmbedded(generatorModificationInfos.getDroop()) : null;
        this.transientReactance = generatorModificationInfos.getTransientReactance() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getTransientReactance()) : null;
        this.stepUpTransformerReactance = generatorModificationInfos.getStepUpTransformerReactance() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getStepUpTransformerReactance()) : null;
        this.voltageRegulationType = generatorModificationInfos.getVoltageRegulationType() != null ? new EnumModificationEmbedded<>(generatorModificationInfos.getVoltageRegulationType()) : null;
        this.regulatingTerminalId = generatorModificationInfos.getRegulatingTerminalId() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalId()) : null;
        this.regulatingTerminalType = generatorModificationInfos.getRegulatingTerminalType() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalType()) : null;
        this.regulatingTerminalVlId = generatorModificationInfos.getRegulatingTerminalVlId() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalVlId()) : null;
        this.qPercent = generatorModificationInfos.getQPercent() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getQPercent()) : null;
        this.reactiveCapabilityCurve = generatorModificationInfos.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(generatorModificationInfos.getReactiveCapabilityCurvePoints());
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

    @Override
    public GeneratorModificationInfos toModificationInfos() {
        return toGeneratorModificationInfosBuilder().build();
    }

    private GeneratorModificationInfos.GeneratorModificationInfosBuilder<?, ?> toGeneratorModificationInfosBuilder() {
        List<ReactiveCapabilityCurveModificationEmbeddable> pointsEmbeddable = !CollectionUtils.isEmpty(reactiveCapabilityCurvePoints) ? reactiveCapabilityCurvePoints : null;
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
                .stashed(getStashed())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connected(toAttributeModification(getConnected()))
                .energySource(toAttributeModification(getEnergySource()))
                .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
                .maxActivePower(toAttributeModification(getMaxActivePower()))
                .minActivePower(toAttributeModification(getMinActivePower()))
                .ratedNominalPower(toAttributeModification(getRatedNominalPower()))
                .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
                .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
                .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
                .plannedActivePowerSetPoint(toAttributeModification(getPlannedActivePowerSetPoint()))
                .marginalCost(toAttributeModification(getMarginalCost()))
                .plannedOutageRate(toAttributeModification(getPlannedOutageRate()))
                .forcedOutageRate(toAttributeModification(getForcedOutageRate()))
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
                .reactiveCapabilityCurvePoints(points)
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
