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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GeneratorCreationInfos;
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

    @Column(name = "marginalCost")
    private Double marginalCost;

    @Column(name = "minimumReactivePower")
    private Double minQ;

    @Column(name = "maximumReactivePower")
    private Double maxQ;

    @Column(name = "participate")
    private Boolean participate;

    @Column(name = "droop")
    private Float droop;

    @Column(name = "transientReactance")
    private Double transientReactance;

    @Column(name = "stepUpTransformerReactance")
    private Double stepUpTransformerReactance;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    public GeneratorCreationEntity(String equipmentId, String equipmentName, EnergySource energySource, String voltageLevelId, String busOrBusbarSectionId,
                                   double minActivePower, double maxActivePower, Double ratedNominalPower, double activePowerSetpoint,
                                   Double reactivePowerSetpoint, boolean voltageRegulationOn, Double voltageSetpoint, Double marginalCost, Double minQ,
                                   Double maxQ, boolean participate, Float droop, Double transientReactance, Double stepUpTransformerReactance,
                                   List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints) {
        super(ModificationType.GENERATOR_CREATION, equipmentId, equipmentName, voltageLevelId, busOrBusbarSectionId);
        this.energySource = energySource;
        this.minActivePower = minActivePower;
        this.maxActivePower = maxActivePower;
        this.ratedNominalPower = ratedNominalPower;
        this.activePowerSetpoint = activePowerSetpoint;
        this.reactivePowerSetpoint = reactivePowerSetpoint;
        this.voltageRegulationOn = voltageRegulationOn;
        this.voltageSetpoint = voltageSetpoint;
        this.marginalCost = marginalCost;
        this.minQ = minQ;
        this.maxQ = maxQ;
        this.participate = participate;
        this.droop = droop;
        this.transientReactance = transientReactance;
        this.stepUpTransformerReactance = stepUpTransformerReactance;
        this.reactiveCapabilityCurvePoints = reactiveCapabilityCurvePoints;
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
        List<ReactiveCapabilityCurveCreationInfos> points = getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getMinQ(),
                        value.getMaxQ(),
                        value.getP()))
                .collect(Collectors.toList());

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
            .marginalCost(getMarginalCost())
            .minQ(getMinQ())
            .participate(getParticipate())
            .droop(getDroop())
            .maxQ(getMaxQ())
            .points(points)
            .transientReactance(getTransientReactance())
            .stepUpTransformerReactance(getStepUpTransformerReactance());
    }
}
