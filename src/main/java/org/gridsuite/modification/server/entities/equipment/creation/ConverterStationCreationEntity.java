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
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.List;
import java.util.stream.Collectors;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Entity
@Table(name = "vscCreation")
public class ConverterStationCreationEntity {
    @Column
    @Id
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
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    public ConverterStationCreationEntity(ConverterStationCreationInfos converterStationCreationInfos) {
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
