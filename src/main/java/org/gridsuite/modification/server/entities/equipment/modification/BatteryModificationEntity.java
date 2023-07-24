/**
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.*;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable.toAttributeModification;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "batteryModification")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "batteryModification_id_fk_constraint"))
public class BatteryModificationEntity extends InjectionModificationEntity {
    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minActivePower")), @AttributeOverride(name = "opType", column = @Column(name = "minActivePowerOp"))
    })
    private DoubleModificationEmbedded minActivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maxActivePower")), @AttributeOverride(name = "opType", column = @Column(name = "maxActivePowerOp"))
    })
    private DoubleModificationEmbedded maxActivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "activePowerSetpoint")), @AttributeOverride(name = "opType", column = @Column(name = "activePowerSetpointOp"))
    })
    private DoubleModificationEmbedded activePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactivePowerSetpoint")), @AttributeOverride(name = "opType", column = @Column(name = "reactivePowerSetpointOp"))
    })
    private DoubleModificationEmbedded reactivePowerSetpoint;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minimumReactivePower")), @AttributeOverride(name = "opType", column = @Column(name = "minimumReactivePowerOp"))
    })
    private DoubleModificationEmbedded minimumReactivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maximumReactivePower")), @AttributeOverride(name = "opType", column = @Column(name = "maximumReactivePowerOp"))
    })
    private DoubleModificationEmbedded maximumReactivePower;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "participate")), @AttributeOverride(name = "opType", column = @Column(name = "participateOp"))
    })
    private BooleanModificationEmbedded participate;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "droop")), @AttributeOverride(name = "opType", column = @Column(name = "droopOp"))
    })
    private FloatModificationEmbedded droop;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "reactiveCapabilityCurve")), @AttributeOverride(name = "opType", column = @Column(name = "reactiveCapabilityCurveOp"))
    })
    private BooleanModificationEmbedded reactiveCapabilityCurve;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveModificationEmbeddable> reactiveCapabilityCurvePoints;

    public BatteryModificationEntity(@NonNull BatteryModificationInfos batteryModificationInfos) {
        super(batteryModificationInfos);
        assignAttributes(batteryModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((BatteryModificationInfos) modificationInfos);
    }

    private void assignAttributes(BatteryModificationInfos batteryModificationInfos) {
        this.minActivePower = new DoubleModificationEmbedded(batteryModificationInfos.getMinActivePower());
        this.maxActivePower = new DoubleModificationEmbedded(batteryModificationInfos.getMaxActivePower());
        this.activePowerSetpoint = new DoubleModificationEmbedded(batteryModificationInfos.getActivePowerSetpoint());
        this.reactivePowerSetpoint = new DoubleModificationEmbedded(batteryModificationInfos.getReactivePowerSetpoint());
        this.minimumReactivePower = new DoubleModificationEmbedded(batteryModificationInfos.getMinimumReactivePower());
        this.maximumReactivePower = new DoubleModificationEmbedded(batteryModificationInfos.getMaximumReactivePower());
        this.participate = new BooleanModificationEmbedded(batteryModificationInfos.getParticipate());
        this.droop = new FloatModificationEmbedded(batteryModificationInfos.getDroop());
        this.reactiveCapabilityCurve = new BooleanModificationEmbedded(batteryModificationInfos.getReactiveCapabilityCurve());
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(batteryModificationInfos.getReactiveCapabilityCurvePoints());
    }

    public static List<ReactiveCapabilityCurveModificationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveModificationInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveModificationEmbeddable(point.getQminP(), point.getOldQminP(),
                        point.getQmaxP(), point.getOldQmaxP(), point.getP(),
                        point.getOldP()))
                .toList();
    }

    @Override
    public BatteryModificationInfos toModificationInfos() {
        return toBatteryModificationInfosBuilder().build();
    }

    private BatteryModificationInfos.BatteryModificationInfosBuilder<?, ?> toBatteryModificationInfosBuilder() {
        List<ReactiveCapabilityCurveModificationEmbeddable> pointsEmbeddable = getReactiveCapabilityCurvePoints();
        List<ReactiveCapabilityCurveModificationInfos> points;
        points = pointsEmbeddable != null ? getReactiveCapabilityCurvePoints()
                .stream()
                .map(value -> new ReactiveCapabilityCurveModificationInfos(value.getQminP(), value.getOldQminP(),
                        value.getQmaxP(), value.getOldQmaxP(),
                        value.getP(), value.getOldP()))
                .collect(Collectors.toList()) : null;
        return BatteryModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .activePowerSetpoint(toAttributeModification(getActivePowerSetpoint()))
                .maxActivePower(toAttributeModification(getMaxActivePower()))
                .minActivePower(toAttributeModification(getMinActivePower()))
                .reactivePowerSetpoint(toAttributeModification(getReactivePowerSetpoint()))
                .minimumReactivePower(toAttributeModification(getMinimumReactivePower()))
                .maximumReactivePower(toAttributeModification(getMaximumReactivePower()))
                .participate(toAttributeModification(getParticipate()))
                .droop(toAttributeModification(getDroop()))
                .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
                .reactiveCapabilityCurvePoints(points);
    }

}
