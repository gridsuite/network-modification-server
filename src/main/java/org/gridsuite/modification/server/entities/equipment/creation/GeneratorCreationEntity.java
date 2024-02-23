/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.EnergySource;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "generatorCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "generatorCreation_id_fk_constraint"))
public class GeneratorCreationEntity extends InjectionCreationEntity {
    @Column(name = "energySource")
    private EnergySource energySource;

    @Column(name = "minP")
    private double minP;

    @Column(name = "maxP")
    private double maxP;

    @Column(name = "ratedS")
    private Double ratedS;

    @Column(name = "targetP")
    private double targetP;

    @Column(name = "targetQ")
    private Double targetQ;

    @Column(name = "voltageRegulationOn")
    private boolean voltageRegulationOn;

    @Column(name = "targetV")
    private Double targetV;

    @Column(name = "plannedActivePowerSetPoint")
    private Double plannedActivePowerSetPoint;

    @Column(name = "marginalCost")
    private Double marginalCost;

    @Column(name = "plannedOutageRate")
    private Double plannedOutageRate;

    @Column(name = "forcedOutageRate")
    private Double forcedOutageRate;

    @Column(name = "minQ")
    private Double minQ;

    @Column(name = "maxQ")
    private Double maxQ;

    @Column(name = "participate")
    private Boolean participate;

    @Column(name = "droop")
    private Float droop;

    @Column(name = "directTransX")
    private Double directTransX;

    @Column(name = "stepUpTransformerX")
    private Double stepUpTransformerX;

    @Column(name = "regulatingTerminalId")
    private String regulatingTerminalId;

    @Column(name = "regulatingTerminalType")
    private String regulatingTerminalType;

    @Column(name = "regulatingTerminalVlId")
    private String regulatingTerminalVlId;

    @Column(name = "qPercent")
    private Double qPercent;

    @Column(name = "reactiveCapabilityCurve")
    private Boolean reactiveCapabilityCurve;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    public GeneratorCreationEntity(@NonNull GeneratorCreationInfos generatorCreationInfos) {
        super(generatorCreationInfos);
        assignAttributes(generatorCreationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GeneratorCreationInfos) modificationInfos);
    }

    private void assignAttributes(GeneratorCreationInfos generatorCreationInfos) {
        this.energySource = generatorCreationInfos.getEnergySource();
        this.minP = generatorCreationInfos.getMinP();
        this.maxP = generatorCreationInfos.getMaxP();
        this.ratedS = generatorCreationInfos.getRatedS();
        this.targetP = generatorCreationInfos.getTargetP();
        this.targetQ = generatorCreationInfos.getTargetQ();
        this.voltageRegulationOn = generatorCreationInfos.isVoltageRegulationOn();
        this.targetV = generatorCreationInfos.getTargetV();
        this.plannedActivePowerSetPoint = generatorCreationInfos.getPlannedActivePowerSetPoint();
        this.marginalCost = generatorCreationInfos.getMarginalCost();
        this.plannedOutageRate = generatorCreationInfos.getPlannedOutageRate();
        this.forcedOutageRate = generatorCreationInfos.getForcedOutageRate();
        this.minQ = generatorCreationInfos.getMinQ();
        this.maxQ = generatorCreationInfos.getMaxQ();
        this.participate = generatorCreationInfos.getParticipate();
        this.droop = generatorCreationInfos.getDroop();
        this.directTransX = generatorCreationInfos.getDirectTransX();
        this.stepUpTransformerX = generatorCreationInfos.getStepUpTransformerX();
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(generatorCreationInfos.getReactiveCapabilityCurvePoints());
        this.regulatingTerminalId = generatorCreationInfos.getRegulatingTerminalId();
        this.regulatingTerminalType = generatorCreationInfos.getRegulatingTerminalType();
        this.regulatingTerminalVlId = generatorCreationInfos.getRegulatingTerminalVlId();
        this.qPercent = generatorCreationInfos.getQPercent();
        this.reactiveCapabilityCurve = generatorCreationInfos.getReactiveCapabilityCurve();
    }

    public static List<ReactiveCapabilityCurveCreationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveCreationInfos> points) {
        return points == null ? null : points.stream()
                .map(point -> new ReactiveCapabilityCurveCreationEmbeddable(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .collect(Collectors.toList());
    }

    @Override
    public GeneratorCreationInfos toModificationInfos() {
        return toGeneratorCreationInfosBuilder().build();
    }

    private GeneratorCreationInfos.GeneratorCreationInfosBuilder<?, ?> toGeneratorCreationInfosBuilder() {
        List<ReactiveCapabilityCurveCreationInfos> points = getReactiveCapabilityCurvePoints() != null ? getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getMinQ(),
                        value.getMaxQ(),
                        value.getP()))
                .collect(Collectors.toList()) : null;

        return GeneratorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .stashed(getStashed())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            // injection
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection())
            .connectionPosition(getConnectionPosition())
            .connected(isConnected())
            // generator
            .energySource(getEnergySource())
            .minP(getMinP())
            .maxP(getMaxP())
            .ratedS(getRatedS())
            .targetP(getTargetP())
            .targetQ(getTargetQ())
            .voltageRegulationOn(isVoltageRegulationOn())
            .targetV(getTargetV())
            .plannedActivePowerSetPoint(getPlannedActivePowerSetPoint())
            .marginalCost(getMarginalCost())
            .plannedOutageRate(getPlannedOutageRate())
            .forcedOutageRate(getForcedOutageRate())
            .minQ(this.getMinQ())
            .participate(getParticipate())
            .droop(getDroop())
            .maxQ(this.getMaxQ())
            .reactiveCapabilityCurvePoints(points)
            .regulatingTerminalId(getRegulatingTerminalId())
            .regulatingTerminalType(getRegulatingTerminalType())
            .regulatingTerminalVlId(getRegulatingTerminalVlId())
            .qPercent(getQPercent())
            .reactiveCapabilityCurve(getReactiveCapabilityCurve())
            .directTransX(getDirectTransX())
            .stepUpTransformerX(getStepUpTransformerX())
            // properties
            .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                   getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
