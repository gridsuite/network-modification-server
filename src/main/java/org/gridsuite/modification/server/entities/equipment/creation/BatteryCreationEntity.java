/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ReactiveCapabilityCurveCreationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "batteryCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "batteryCreation_id_fk_constraint"))
public class BatteryCreationEntity extends InjectionCreationEntity {

    @Column(name = "connectionName")
    private String connectionName;

    @Column(name = "connectionDirection")
    private ConnectablePosition.Direction connectionDirection;

    @Column(name = "connectionPosition")
    private Integer connectionPosition;

    @Column(name = "minActivePower")
    private double minActivePower;

    @Column(name = "maxActivePower")
    private double maxActivePower;

    @Column(name = "reactiveCapabilityCurve")
    private Boolean reactiveCapabilityCurve;

    @Column(name = "minimumReactivePower")
    private Double minimumReactivePower;

    @Column(name = "maximumReactivePower")
    private Double maximumReactivePower;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    @Column(name = "activePowerSetpoint")
    private double activePowerSetpoint;

    @Column(name = "reactivePowerSetpoint")
    private Double reactivePowerSetpoint;

    @Column(name = "participate")
    private Boolean participate;

    @Column(name = "droop")
    private Float droop;

    public BatteryCreationEntity(@NonNull BatteryCreationInfos batteryCreationInfos) {
        super(batteryCreationInfos);
        assignAttributes(batteryCreationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BatteryCreationInfos) modificationInfos);
    }

    private void assignAttributes(BatteryCreationInfos batteryCreationInfos) {
        this.connectionName = batteryCreationInfos.getConnectionName();
        this.connectionDirection = batteryCreationInfos.getConnectionDirection();
        this.connectionPosition = batteryCreationInfos.getConnectionPosition();
        this.minActivePower = batteryCreationInfos.getMinActivePower();
        this.maxActivePower = batteryCreationInfos.getMaxActivePower();
        this.reactiveCapabilityCurve = batteryCreationInfos.getReactiveCapabilityCurve();
        this.minimumReactivePower = batteryCreationInfos.getMinimumReactivePower();
        this.maximumReactivePower = batteryCreationInfos.getMaximumReactivePower();
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(batteryCreationInfos.getReactiveCapabilityCurvePoints());
        this.activePowerSetpoint = batteryCreationInfos.getActivePowerSetpoint();
        this.reactivePowerSetpoint = batteryCreationInfos.getReactivePowerSetpoint();
        this.participate = batteryCreationInfos.getParticipate();
        this.droop = batteryCreationInfos.getDroop();
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
    public BatteryCreationInfos toModificationInfos() {
        return toBatteryCreationInfosBuilder().build();
    }

    private BatteryCreationInfos.BatteryCreationInfosBuilder<?, ?> toBatteryCreationInfosBuilder() {
        List<ReactiveCapabilityCurveCreationEmbeddable> pointsEmbeddable = getReactiveCapabilityCurvePoints();
        List<ReactiveCapabilityCurveCreationInfos> points = pointsEmbeddable != null ? getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getQminP(),
                        value.getQmaxP(),
                        value.getP()))
                .collect(Collectors.toList()) : null;

        return BatteryCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .voltageLevelId(getVoltageLevelId())
                .busOrBusbarSectionId(getBusOrBusbarSectionId())
                .connectionName(getConnectionName())
                .connectionDirection(getConnectionDirection())
                .connectionPosition(getConnectionPosition())
                .minActivePower(getMinActivePower())
                .maxActivePower(getMaxActivePower())
                .reactiveCapabilityCurve(this.getReactiveCapabilityCurve())
                .minimumReactivePower(this.getMinimumReactivePower())
                .maximumReactivePower(this.getMaximumReactivePower())
                .reactiveCapabilityCurvePoints(points)
                .activePowerSetpoint(getActivePowerSetpoint())
                .reactivePowerSetpoint(getReactivePowerSetpoint())
                .participate(getParticipate())
                .droop(getDroop());
    }

}