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

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
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
@Table(name = "converterStationCreationEntity")
public class ConverterStationCreationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private String converterStationId;

    @Column
    private String converterStationName;

    @Column
    private Float lossFactor;

    @Column
    private Double minimumReactivePower;

    @Column
    private Double maximumReactivePower;

    @Column
    private Double reactivePower;

    @Column
    private Boolean voltageRegulationOn;

    @Column
    private Double voltage;

    @ElementCollection
    @CollectionTable(name = "converter_station_creation_rcc_points")
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    public ConverterStationCreationEntity(ConverterStationCreationInfos converterStationCreationInfos) {
        this.id = UUID.randomUUID();
        assignAttributes(converterStationCreationInfos);
    }

    private void assignAttributes(ConverterStationCreationInfos converterStationCreationInfos) {
        this.converterStationId = converterStationCreationInfos.getEquipmentId();
        this.converterStationName = converterStationCreationInfos.getEquipmentName();
        this.lossFactor = converterStationCreationInfos.getLossFactor();
        this.minimumReactivePower = converterStationCreationInfos.getMinimumReactivePower();
        this.maximumReactivePower = converterStationCreationInfos.getMaximumReactivePower();
        this.reactivePower = converterStationCreationInfos.getReactivePower();
        this.voltageRegulationOn = converterStationCreationInfos.getVoltageRegulationOn();
        this.voltage = converterStationCreationInfos.getVoltage();
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(converterStationCreationInfos.getReactiveCapabilityCurvePoints());
    }

    public ConverterStationCreationInfos toConverterStationInfos() {
        return ConverterStationCreationInfos.builder()
                .equipmentId(getConverterStationId())
                .equipmentName(getConverterStationName())
                .lossFactor(getLossFactor())
                .minimumReactivePower(getMinimumReactivePower())
                .maximumReactivePower(getMaximumReactivePower())
                .reactivePower(getReactivePower())
                .voltageRegulationOn(getVoltageRegulationOn())
                .voltage(getVoltage())
                .reactiveCapabilityCurvePoints(toReactiveCapabilityCurveInfos(getReactiveCapabilityCurvePoints()))
                .build();
    }

    private static List<ReactiveCapabilityCurveCreationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveCreationInfos> points) {
        return points == null ? null : points.stream()
                .map(point -> new ReactiveCapabilityCurveCreationEmbeddable(point.getQminP(),
                        point.getQmaxP(),
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
                        .qmaxP(pointEmbeddable.getQmaxP())
                        .qminP(pointEmbeddable.getQminP())
                        .build())
                .collect(Collectors.toList());
    }
}
