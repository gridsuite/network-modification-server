/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import jakarta.persistence.CollectionTable;
import jakarta.persistence.Column;
import jakarta.persistence.ElementCollection;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "converterStationCreation")
public class ConverterStationCreationEntity extends InjectionCreationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private Float lossFactor;

    @Column
    private Double minQ;

    @Column
    private Double maxQ;

    @Column
    private Double reactivePowerSetpoint;

    @Column
    private Boolean voltageRegulationOn;

    @Column
    private Double voltageSetpoint;

    @ElementCollection
    @CollectionTable(name = "converter_station_creation_rcc_points")
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    @Column
    private Boolean reactiveCapabilityCurve;

    public ConverterStationCreationEntity(ConverterStationCreationInfos converterStationCreationInfos) {
        super(converterStationCreationInfos);
        assignAttributes(converterStationCreationInfos);
    }

    private void assignAttributes(ConverterStationCreationInfos converterStationCreationInfos) {
        this.lossFactor = converterStationCreationInfos.getLossFactor();
        this.minQ = converterStationCreationInfos.getMinQ();
        this.maxQ = converterStationCreationInfos.getMaxQ();
        this.reactivePowerSetpoint = converterStationCreationInfos.getReactivePowerSetpoint();
        this.voltageRegulationOn = converterStationCreationInfos.getVoltageRegulationOn();
        this.voltageSetpoint = converterStationCreationInfos.getVoltageSetpoint();
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(converterStationCreationInfos.getReactiveCapabilityCurvePoints());
        this.reactiveCapabilityCurve = converterStationCreationInfos.getReactiveCapabilityCurve();
    }

    public ConverterStationCreationInfos toConverterStationInfos() {
        return ConverterStationCreationInfos.builder()
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                // Injection
                .voltageLevelId(getVoltageLevelId())
                .busOrBusbarSectionId(getBusOrBusbarSectionId())
                .connectionName(getConnectionName())
                .connectionPosition(getConnectionPosition())
                .connectionDirection(getConnectionDirection())
                .terminalConnected(isTerminalConnected())
                // ConverterStation
                .lossFactor(getLossFactor())
                .minQ(getMinQ())
                .maxQ(getMaxQ())
                .reactivePowerSetpoint(getReactivePowerSetpoint())
                .voltageRegulationOn(getVoltageRegulationOn())
                .voltageSetpoint(getVoltageSetpoint())
                .reactiveCapabilityCurvePoints(toReactiveCapabilityCurveInfos(getReactiveCapabilityCurvePoints()))
                .reactiveCapabilityCurve(getReactiveCapabilityCurve())
                .build();
    }

    private static List<ReactiveCapabilityCurveCreationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveCreationInfos> points) {
        return points == null ? null : points.stream()
                .map(point -> new ReactiveCapabilityCurveCreationEmbeddable(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .collect(Collectors.toList());
    }

    private static List<ReactiveCapabilityCurveCreationInfos> toReactiveCapabilityCurveInfos(List<ReactiveCapabilityCurveCreationEmbeddable> pointsEmbeddable) {
        if (pointsEmbeddable == null) {
            return null;
        }

        return pointsEmbeddable.stream()
                .map(pointEmbeddable -> ReactiveCapabilityCurveCreationInfos.builder()
                        .p(pointEmbeddable.getP())
                        .maxQ(pointEmbeddable.getMaxQ())
                        .minQ(pointEmbeddable.getMinQ())
                        .build())
                .collect(Collectors.toList());
    }
}
