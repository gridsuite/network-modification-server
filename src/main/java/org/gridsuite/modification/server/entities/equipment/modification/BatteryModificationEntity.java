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

import jakarta.persistence.*;
import org.springframework.util.CollectionUtils;

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
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minP")), @AttributeOverride(name = "opType", column = @Column(name = "minpOp"))
    })
    private DoubleModificationEmbedded minP;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maxP")), @AttributeOverride(name = "opType", column = @Column(name = "maxpOp"))
    })
    private DoubleModificationEmbedded maxP;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "targetP")), @AttributeOverride(name = "opType", column = @Column(name = "targetpOp"))
    })
    private DoubleModificationEmbedded targetP;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "targetQ")), @AttributeOverride(name = "opType", column = @Column(name = "targetqOp"))
    })
    private DoubleModificationEmbedded targetQ;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "participate")), @AttributeOverride(name = "opType", column = @Column(name = "participateOp"))
    })
    private BooleanModificationEmbedded participate;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "droop")), @AttributeOverride(name = "opType", column = @Column(name = "droopOp"))
    })
    private FloatModificationEmbedded droop;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "minQ")), @AttributeOverride(name = "opType", column = @Column(name = "minqOp"))
    })
    private DoubleModificationEmbedded minQ;

    @Embedded
    @AttributeOverrides(value = {@AttributeOverride(name = "value", column = @Column(name = "maxQ")), @AttributeOverride(name = "opType", column = @Column(name = "maxqOp"))
    })
    private DoubleModificationEmbedded maxQ;

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
        this.minP = batteryModificationInfos.getMinP() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getMinP()) : null;
        this.maxP = batteryModificationInfos.getMaxP() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getMaxP()) : null;
        this.targetP = batteryModificationInfos.getTargetP() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getTargetP()) : null;
        this.targetQ = batteryModificationInfos.getTargetQ() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getTargetQ()) : null;
        this.minQ = batteryModificationInfos.getMinQ() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getMinQ()) : null;
        this.maxQ = batteryModificationInfos.getMaxQ() != null ? new DoubleModificationEmbedded(batteryModificationInfos.getMaxQ()) : null;
        this.participate = batteryModificationInfos.getParticipate() != null ? new BooleanModificationEmbedded(batteryModificationInfos.getParticipate()) : null;
        this.droop = batteryModificationInfos.getDroop() != null ? new FloatModificationEmbedded(batteryModificationInfos.getDroop()) : null;
        this.reactiveCapabilityCurve = batteryModificationInfos.getReactiveCapabilityCurve() != null ? new BooleanModificationEmbedded(batteryModificationInfos.getReactiveCapabilityCurve()) : null;
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(batteryModificationInfos.getReactiveCapabilityCurvePoints());
    }

    public static List<ReactiveCapabilityCurveModificationEmbeddable> toEmbeddablePoints(
            List<ReactiveCapabilityCurveModificationInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveModificationEmbeddable(point.getMinQ(), point.getOldMinQ(),
                        point.getMaxQ(), point.getOldMaxQ(), point.getP(),
                        point.getOldP()))
                .toList();
    }

    @Override
    public BatteryModificationInfos toModificationInfos() {
        return toBatteryModificationInfosBuilder().build();
    }

    private BatteryModificationInfos.BatteryModificationInfosBuilder<?, ?> toBatteryModificationInfosBuilder() {
        List<ReactiveCapabilityCurveModificationEmbeddable> pointsEmbeddable = !CollectionUtils.isEmpty(reactiveCapabilityCurvePoints) ? reactiveCapabilityCurvePoints : null;
        List<ReactiveCapabilityCurveModificationInfos> points = pointsEmbeddable != null ? getReactiveCapabilityCurvePoints()
            .stream()
            .map(value -> new ReactiveCapabilityCurveModificationInfos(value.getMinQ(), value.getOldMinQ(),
                value.getMaxQ(), value.getOldMaxQ(),
                value.getP(), value.getOldP()))
            .collect(Collectors.toList()) : null;

        return BatteryModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .equipmentId(getEquipmentId())
                .equipmentName(AttributeModification.toAttributeModification(getEquipmentNameValue(), getEquipmentNameOp()))
                .voltageLevelId(AttributeModification.toAttributeModification(getVoltageLevelIdValue(), getVoltageLevelIdOp()))
                .busOrBusbarSectionId(AttributeModification.toAttributeModification(getBusOrBusbarSectionIdValue(), getBusOrBusbarSectionIdOp()))
                .connected(toAttributeModification(getConnected()))
                .targetP(toAttributeModification(getTargetP()))
                .maxP(toAttributeModification(getMaxP()))
                .minP(toAttributeModification(getMinP()))
                .targetQ(toAttributeModification(getTargetQ()))
                .minQ(toAttributeModification(getMinQ()))
                .maxQ(toAttributeModification(getMaxQ()))
                .participate(toAttributeModification(getParticipate()))
                .droop(toAttributeModification(getDroop()))
                .reactiveCapabilityCurve(toAttributeModification(getReactiveCapabilityCurve()))
                .reactiveCapabilityCurvePoints(points)
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }
}
