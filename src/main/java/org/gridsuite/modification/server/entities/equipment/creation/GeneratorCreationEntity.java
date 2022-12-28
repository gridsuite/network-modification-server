/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import javax.persistence.*;
import java.util.ArrayList;
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

    @Column(name = "startupCost")
    private Double startupCost;

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

    @Column(name = "connectionName")
    private String connectionName;

    @Column(name = "connectionDirection")
    private ConnectablePosition.Direction connectionDirection;

    @Column(name = "connectionPosition")
    private Integer connectionPosition;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    public GeneratorCreationEntity(String equipmentId, String equipmentName, EnergySource energySource, String voltageLevelId, String busOrBusbarSectionId,
                                   double minActivePower, double maxActivePower, Double ratedNominalPower, double activePowerSetpoint,
                                   Double reactivePowerSetpoint, boolean voltageRegulationOn, Double voltageSetpoint, Double plannedActivePowerSetPoint, Double startupCost,
                                   Double marginalCost, Double plannedOutageRate, Double forcedOutageRate, Double minQ,
                                   Double maxQ, boolean participate, Float droop, Double transientReactance, Double stepUpTransformerReactance,
                                   List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints, String regulatingTerminalId, String regulatingTerminalType, String regulatingTerminalVlId,
                                   Double qPercent, Boolean reactiveCapabilityCurve, String connectionName, ConnectablePosition.Direction connectionDirection, Integer connectionPosition) {
        super(ModificationType.GENERATOR_CREATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.energySource = energySource;
        this.minActivePower = minActivePower;
        this.maxActivePower = maxActivePower;
        this.ratedNominalPower = ratedNominalPower;
        this.activePowerSetpoint = activePowerSetpoint;
        this.reactivePowerSetpoint = reactivePowerSetpoint;
        this.voltageRegulationOn = voltageRegulationOn;
        this.voltageSetpoint = voltageSetpoint;
        this.plannedActivePowerSetPoint = plannedActivePowerSetPoint;
        this.startupCost = startupCost;
        this.marginalCost = marginalCost;
        this.plannedOutageRate = plannedOutageRate;
        this.forcedOutageRate = forcedOutageRate;
        this.minimumReactivePower = minQ;
        this.maximumReactivePower = maxQ;
        this.participate = participate;
        this.droop = droop;
        this.transientReactance = transientReactance;
        this.stepUpTransformerReactance = stepUpTransformerReactance;
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
        this.regulatingTerminalId = regulatingTerminalId;
        this.regulatingTerminalType = regulatingTerminalType;
        this.regulatingTerminalVlId = regulatingTerminalVlId;
        this.qPercent = qPercent;
        this.reactiveCapabilityCurve = reactiveCapabilityCurve;
        this.connectionDirection = connectionDirection;
        this.connectionName = connectionName;
        this.connectionPosition = connectionPosition;
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
            .type(ModificationType.valueOf(getType()))
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
            .startupCost(getStartupCost())
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

    @Override
    public void cloneWithIdsToNull() {
        super.cloneWithIdsToNull();
        this.reactiveCapabilityCurvePoints = new ArrayList<>(reactiveCapabilityCurvePoints);
    }
}
