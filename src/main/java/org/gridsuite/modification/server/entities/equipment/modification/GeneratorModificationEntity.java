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
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.GeneratorModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageRegulationType;
import org.gridsuite.modification.server.dto.DTOUtils;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;
import org.springframework.util.CollectionUtils;

import java.util.List;

import static org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable.toEmbeddablePoints;
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
        @AttributeOverride(name = "value", column = @Column(name = "minP")),
        @AttributeOverride(name = "opType", column = @Column(name = "minpOp"))
    })
    private DoubleModificationEmbedded minP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxP")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxpOp"))
    })
    private DoubleModificationEmbedded maxP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "ratedS")),
        @AttributeOverride(name = "opType", column = @Column(name = "ratedsOp"))
    })
    private DoubleModificationEmbedded ratedS;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "targetP")),
        @AttributeOverride(name = "opType", column = @Column(name = "targetpOp"))
    })
    private DoubleModificationEmbedded targetP;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "targetQ")),
        @AttributeOverride(name = "opType", column = @Column(name = "targetqOp"))
    })
    private DoubleModificationEmbedded targetQ;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "voltageRegulationOn")),
        @AttributeOverride(name = "opType", column = @Column(name = "voltageRegulationOnOp"))
    })
    private BooleanModificationEmbedded voltageRegulationOn;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "targetV")),
        @AttributeOverride(name = "opType", column = @Column(name = "targetvOp"))
    })
    private DoubleModificationEmbedded targetV;

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
        @AttributeOverride(name = "value", column = @Column(name = "minQ")),
        @AttributeOverride(name = "opType", column = @Column(name = "minqOp"))
    })
    private DoubleModificationEmbedded minQ;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "maxQ")),
        @AttributeOverride(name = "opType", column = @Column(name = "maxqOp"))
    })
    private DoubleModificationEmbedded maxQ;

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
        @AttributeOverride(name = "value", column = @Column(name = "directTransX")),
        @AttributeOverride(name = "opType", column = @Column(name = "directTransxOp"))
    })
    private DoubleModificationEmbedded directTransX;

    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "stepUpTransformerX")),
        @AttributeOverride(name = "opType", column = @Column(name = "stepUpTransformerxOp"))
    })
    private DoubleModificationEmbedded stepUpTransformerX;

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
        this.minP = generatorModificationInfos.getMinP() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMinP()) : null;
        this.maxP = generatorModificationInfos.getMaxP() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMaxP()) : null;
        this.ratedS = generatorModificationInfos.getRatedS() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getRatedS()) : null;
        this.targetP = generatorModificationInfos.getTargetP() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getTargetP()) : null;
        this.targetQ = generatorModificationInfos.getTargetQ() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getTargetQ()) : null;
        this.voltageRegulationOn = generatorModificationInfos.getVoltageRegulationOn() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getVoltageRegulationOn()) : null;
        this.targetV = generatorModificationInfos.getTargetV() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getTargetV()) : null;
        this.plannedActivePowerSetPoint = generatorModificationInfos.getPlannedActivePowerSetPoint() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getPlannedActivePowerSetPoint()) : null;
        this.marginalCost = generatorModificationInfos.getMarginalCost() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMarginalCost()) : null;
        this.plannedOutageRate = generatorModificationInfos.getPlannedOutageRate() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getPlannedOutageRate()) : null;
        this.forcedOutageRate = generatorModificationInfos.getForcedOutageRate() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getForcedOutageRate()) : null;
        this.minQ = generatorModificationInfos.getMinQ() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMinQ()) : null;
        this.maxQ = generatorModificationInfos.getMaxQ() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getMaxQ()) : null;
        this.participate = generatorModificationInfos.getParticipate() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getParticipate()) : null;
        this.droop = generatorModificationInfos.getDroop() != null ? new FloatModificationEmbedded(generatorModificationInfos.getDroop()) : null;
        this.directTransX = generatorModificationInfos.getDirectTransX() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getDirectTransX()) : null;
        this.stepUpTransformerX = generatorModificationInfos.getStepUpTransformerX() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getStepUpTransformerX()) : null;
        this.voltageRegulationType = generatorModificationInfos.getVoltageRegulationType() != null ? new EnumModificationEmbedded<>(generatorModificationInfos.getVoltageRegulationType()) : null;
        this.regulatingTerminalId = generatorModificationInfos.getRegulatingTerminalId() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalId()) : null;
        this.regulatingTerminalType = generatorModificationInfos.getRegulatingTerminalType() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalType()) : null;
        this.regulatingTerminalVlId = generatorModificationInfos.getRegulatingTerminalVlId() != null ? new StringModificationEmbedded(generatorModificationInfos.getRegulatingTerminalVlId()) : null;
        this.qPercent = generatorModificationInfos.getQPercent() != null ? new DoubleModificationEmbedded(generatorModificationInfos.getQPercent()) : null;
        this.reactiveCapabilityCurve = generatorModificationInfos.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(generatorModificationInfos.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(generatorModificationInfos.getReactiveCapabilityCurvePoints());
    }

    @Override
    public GeneratorModificationInfos toModificationInfos() {
        return toGeneratorModificationInfosBuilder().build();
    }

    private GeneratorModificationInfos.GeneratorModificationInfosBuilder<?, ?> toGeneratorModificationInfosBuilder() {
        return GeneratorModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connectionName(toAttributeModification(getConnectionName()))
                .connectionDirection(toAttributeModification(getConnectionDirection()))
                .connectionPosition(toAttributeModification(getConnectionPosition()))
                .terminalConnected(toAttributeModification(getTerminalConnected()))
                .energySource(toAttributeModification(getEnergySource()))
                .targetP(toAttributeModification(getTargetP()))
                .maxP(toAttributeModification(getMaxP()))
                .minP(toAttributeModification(getMinP()))
                .ratedS(toAttributeModification(getRatedS()))
                .targetQ(toAttributeModification(getTargetQ()))
                .voltageRegulationOn(toAttributeModification(getVoltageRegulationOn()))
                .targetV(toAttributeModification(getTargetV()))
                .plannedActivePowerSetPoint(toAttributeModification(getPlannedActivePowerSetPoint()))
                .marginalCost(toAttributeModification(getMarginalCost()))
                .plannedOutageRate(toAttributeModification(getPlannedOutageRate()))
                .forcedOutageRate(toAttributeModification(getForcedOutageRate()))
                .minQ(toAttributeModification(getMinQ()))
                .maxQ(toAttributeModification(getMaxQ()))
                .participate(toAttributeModification(getParticipate()))
                .droop(toAttributeModification(getDroop()))
                .directTransX(toAttributeModification(getDirectTransX()))
                .stepUpTransformerX(toAttributeModification(getStepUpTransformerX()))
                .voltageRegulationType(toAttributeModification(getVoltageRegulationType()))
                .regulatingTerminalId(toAttributeModification(getRegulatingTerminalId()))
                .regulatingTerminalType(toAttributeModification(getRegulatingTerminalType()))
                .regulatingTerminalVlId(toAttributeModification(getRegulatingTerminalVlId()))
                .qPercent(toAttributeModification(getQPercent()))
                .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
                .reactiveCapabilityCurvePoints(DTOUtils.toReactiveCapabilityCurvePointsModificationInfos(getReactiveCapabilityCurvePoints()))
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
