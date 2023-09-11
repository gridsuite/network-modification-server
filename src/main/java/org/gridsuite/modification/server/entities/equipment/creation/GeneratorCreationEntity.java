/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.EnergySource;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import javax.persistence.*;
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

    @Column(name = "minActivePower")
    private double minActivePower;

    @Column(name = "maxActivePower")
    private double maxActivePower;

    @Column(name = "ratedNominalPower")
    private Double ratedNominalPower;

    @Column(name = "activePowerSetpoint")
    private double activePowerSetpoint;

    @Column(name = "reactivePowerSetpoint")
    private Double reactivePowerSetpoint;

    @Column(name = "voltageRegulationOn")
    private boolean voltageRegulationOn;

    @Column(name = "voltageSetpoint")
    private Double voltageSetpoint;

    @Column(name = "plannedActivePowerSetPoint")
    private Double plannedActivePowerSetPoint;

    @Column(name = "marginalCost")
    private Double marginalCost;

    @Column(name = "plannedOutageRate")
    private Double plannedOutageRate;

    @Column(name = "forcedOutageRate")
    private Double forcedOutageRate;

    @Column(name = "minimumReactivePower")
    private Double minimumReactivePower;

    @Column(name = "maximumReactivePower")
    private Double maximumReactivePower;

    @Column(name = "participate")
    private Boolean participate;

    @Column(name = "droop")
    private Float droop;

    @Column(name = "transientReactance")
    private Double transientReactance;

    @Column(name = "stepUpTransformerReactance")
    private Double stepUpTransformerReactance;

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
        this.minActivePower = generatorCreationInfos.getMinActivePower();
        this.maxActivePower = generatorCreationInfos.getMaxActivePower();
        this.ratedNominalPower = generatorCreationInfos.getRatedNominalPower();
        this.activePowerSetpoint = generatorCreationInfos.getActivePowerSetpoint();
        this.reactivePowerSetpoint = generatorCreationInfos.getReactivePowerSetpoint();
        this.voltageRegulationOn = generatorCreationInfos.isVoltageRegulationOn();
        this.voltageSetpoint = generatorCreationInfos.getVoltageSetpoint();
        this.plannedActivePowerSetPoint = generatorCreationInfos.getPlannedActivePowerSetPoint();
        this.marginalCost = generatorCreationInfos.getMarginalCost();
        this.plannedOutageRate = generatorCreationInfos.getPlannedOutageRate();
        this.forcedOutageRate = generatorCreationInfos.getForcedOutageRate();
        this.minimumReactivePower = generatorCreationInfos.getMinimumReactivePower();
        this.maximumReactivePower = generatorCreationInfos.getMaximumReactivePower();
        this.participate = generatorCreationInfos.getParticipate();
        this.droop = generatorCreationInfos.getDroop();
        this.transientReactance = generatorCreationInfos.getTransientReactance();
        this.stepUpTransformerReactance = generatorCreationInfos.getStepUpTransformerReactance();
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
                .map(point -> new ReactiveCapabilityCurveCreationEmbeddable(point.getQminP(),
                        point.getQmaxP(),
                        point.getP()))
                .collect(Collectors.toList());
    }

    @Override
    public GeneratorCreationInfos toModificationInfos() {
        return toGeneratorCreationInfosBuilder().build();
    }

    private GeneratorCreationInfos.GeneratorCreationInfosBuilder<?, ?> toGeneratorCreationInfosBuilder() {
        List<ReactiveCapabilityCurveCreationEmbeddable> pointsEmbeddable = getReactiveCapabilityCurvePoints();
        List<ReactiveCapabilityCurveCreationInfos> points = pointsEmbeddable != null ? getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getQminP(),
                        value.getQmaxP(),
                        value.getP()))
                .collect(Collectors.toList()) : null;

        return GeneratorCreationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .equipmentId(getEquipmentId())
            .equipmentName(getEquipmentName())
            .voltageLevelId(getVoltageLevelId())
            .busOrBusbarSectionId(getBusOrBusbarSectionId())
            .energySource(getEnergySource())
            .minActivePower(getMinActivePower())
            .maxActivePower(getMaxActivePower())
            .ratedNominalPower(getRatedNominalPower())
            .activePowerSetpoint(getActivePowerSetpoint())
            .reactivePowerSetpoint(getReactivePowerSetpoint())
            .voltageRegulationOn(isVoltageRegulationOn())
            .voltageSetpoint(getVoltageSetpoint())
            .plannedActivePowerSetPoint(getPlannedActivePowerSetPoint())
            .marginalCost(getMarginalCost())
            .plannedOutageRate(getPlannedOutageRate())
            .forcedOutageRate(getForcedOutageRate())
            .minimumReactivePower(this.getMinimumReactivePower())
            .participate(getParticipate())
            .droop(getDroop())
            .maximumReactivePower(this.getMaximumReactivePower())
            .reactiveCapabilityCurvePoints(points)
            .regulatingTerminalId(getRegulatingTerminalId())
            .regulatingTerminalType(getRegulatingTerminalType())
            .regulatingTerminalVlId(getRegulatingTerminalVlId())
            .qPercent(getQPercent())
            .reactiveCapabilityCurve(getReactiveCapabilityCurve())
            .transientReactance(getTransientReactance())
            .stepUpTransformerReactance(getStepUpTransformerReactance())
            .connectionName(getConnectionName())
            .connectionDirection(getConnectionDirection())
            .connectionPosition(getConnectionPosition());
    }
}
