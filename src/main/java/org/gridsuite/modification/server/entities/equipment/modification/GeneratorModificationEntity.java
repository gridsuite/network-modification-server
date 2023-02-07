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
import lombok.NonNull;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;

import javax.persistence.*;
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
        @AttributeOverride(name = "value", column = @Column(name = "plannedActivePowerSetPoint")),
        @AttributeOverride(name = "opType", column = @Column(name = "plannedActivePowerSetPointOp"))
    })
    DoubleModificationEmbedded plannedActivePowerSetPoint;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "startupCost")),
        @AttributeOverride(name = "opType", column = @Column(name = "startupCostOp"))
    })
    DoubleModificationEmbedded startupCost;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "marginalCost")),
        @AttributeOverride(name = "opType", column = @Column(name = "marginalCostOp"))
    })
    DoubleModificationEmbedded marginalCost;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "plannedOutageRate")),
        @AttributeOverride(name = "opType", column = @Column(name = "plannedOutageRateOp"))
    })
    DoubleModificationEmbedded plannedOutageRate;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "forcedOutageRate")),
        @AttributeOverride(name = "opType", column = @Column(name = "forcedOutageRateOp"))
    })
    DoubleModificationEmbedded forcedOutageRate;

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
        this.energySource = new EnumModificationEmbedded<>(generatorModificationInfos.getEnergySource());
        this.minActivePower = new DoubleModificationEmbedded(generatorModificationInfos.getMinActivePower());
        this.maxActivePower = new DoubleModificationEmbedded(generatorModificationInfos.getMaxActivePower());
        this.ratedNominalPower = new DoubleModificationEmbedded(generatorModificationInfos.getRatedNominalPower());
        this.activePowerSetpoint = new DoubleModificationEmbedded(generatorModificationInfos.getActivePowerSetpoint());
        this.reactivePowerSetpoint = new DoubleModificationEmbedded(generatorModificationInfos.getReactivePowerSetpoint());
        this.voltageRegulationOn = new BooleanModificationEmbedded(generatorModificationInfos.getVoltageRegulationOn());
        this.voltageSetpoint = new DoubleModificationEmbedded(generatorModificationInfos.getVoltageSetpoint());
        this.plannedActivePowerSetPoint = new DoubleModificationEmbedded(generatorModificationInfos.getPlannedActivePowerSetPoint());
        this.startupCost = new DoubleModificationEmbedded(generatorModificationInfos.getStartupCost());
        this.marginalCost = new DoubleModificationEmbedded(generatorModificationInfos.getMarginalCost());
        this.plannedOutageRate = new DoubleModificationEmbedded(generatorModificationInfos.getPlannedOutageRate());
        this.forcedOutageRate = new DoubleModificationEmbedded(generatorModificationInfos.getForcedOutageRate());
        this.minimumReactivePower = new DoubleModificationEmbedded(generatorModificationInfos.getMinimumReactivePower());
        this.maximumReactivePower = new DoubleModificationEmbedded(generatorModificationInfos.getMaximumReactivePower());
        this.participate = new BooleanModificationEmbedded(generatorModificationInfos.getParticipate());
        this.droop = new FloatModificationEmbedded(generatorModificationInfos.getDroop());
        this.transientReactance = new DoubleModificationEmbedded(generatorModificationInfos.getTransientReactance());
        this.stepUpTransformerReactance = new DoubleModificationEmbedded(generatorModificationInfos.getStepUpTransformerReactance());
        this.voltageRegulationType = new EnumModificationEmbedded<>(generatorModificationInfos.getVoltageRegulationType());
        this.regulatingTerminalId = new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalId());
        this.regulatingTerminalType = new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalType());
        this.regulatingTerminalVlId = new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalVlId());
        this.qPercent = new DoubleModificationEmbedded(generatorModificationInfos.getQPercent());
        this.reactiveCapabilityCurve = new BooleanModificationEmbedded(generatorModificationInfos.getReactiveCapabilityCurve());
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
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .energySource(toAttributeModification(getEnergySource()))
                .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
                .maxActivePower(toAttributeModification(getMaxActivePower()))
                .minActivePower(toAttributeModification(getMinActivePower()))
                .ratedNominalPower(toAttributeModification(getRatedNominalPower()))
                .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
                .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
                .voltageSetpoint(toAttributeModification(getVoltageSetpoint()))
                .plannedActivePowerSetPoint(toAttributeModification(getPlannedActivePowerSetPoint()))
                .startupCost(toAttributeModification(getStartupCost()))
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
                .reactiveCapabilityCurvePoints(points);
    }
}
