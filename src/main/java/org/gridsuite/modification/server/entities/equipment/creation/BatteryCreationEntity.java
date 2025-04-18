/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.DTOUtils;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.List;

import static org.gridsuite.modification.server.entities.equipment.creation.ReactiveCapabilityCurveCreationEmbeddable.toEmbeddablePoints;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "batteryCreation")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "batteryCreation_id_fk_constraint"))
public class BatteryCreationEntity extends InjectionCreationEntity {

    @Column(name = "minP")
    private double minP;

    @Column(name = "maxP")
    private double maxP;

    @Column(name = "reactiveCapabilityCurve")
    private Boolean reactiveCapabilityCurve;

    @Column(name = "minQ")
    private Double minQ;

    @Column(name = "maxQ")
    private Double maxQ;

    @ElementCollection
    @CollectionTable
    private List<ReactiveCapabilityCurveCreationEmbeddable> reactiveCapabilityCurvePoints;

    @Column(name = "targetP")
    private double targetP;

    @Column(name = "targetQ")
    private Double targetQ;

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
        this.minP = batteryCreationInfos.getMinP();
        this.maxP = batteryCreationInfos.getMaxP();
        this.reactiveCapabilityCurve = batteryCreationInfos.getReactiveCapabilityCurve();
        this.minQ = batteryCreationInfos.getMinQ();
        this.maxQ = batteryCreationInfos.getMaxQ();
        this.reactiveCapabilityCurvePoints = toEmbeddablePoints(batteryCreationInfos.getReactiveCapabilityCurvePoints());
        this.targetP = batteryCreationInfos.getTargetP();
        this.targetQ = batteryCreationInfos.getTargetQ();
        this.participate = batteryCreationInfos.getParticipate();
        this.droop = batteryCreationInfos.getDroop();
    }

    @Override
    public BatteryCreationInfos toModificationInfos() {
        return toBatteryCreationInfosBuilder().build();
    }

    private BatteryCreationInfos.BatteryCreationInfosBuilder<?, ?> toBatteryCreationInfosBuilder() {
        return BatteryCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                // injection
                .voltageLevelId(getVoltageLevelId())
                .busOrBusbarSectionId(getBusOrBusbarSectionId())
                .connectionName(getConnectionName())
                .connectionDirection(getConnectionDirection())
                .connectionPosition(getConnectionPosition())
                .terminalConnected(isTerminalConnected())
                // battery
                .minP(getMinP())
                .maxP(getMaxP())
                .reactiveCapabilityCurve(this.getReactiveCapabilityCurve())
                .minQ(this.getMinQ())
                .maxQ(this.getMaxQ())
                .reactiveCapabilityCurvePoints(DTOUtils.toReactiveCapabilityCurvePointsCreationInfos(getReactiveCapabilityCurvePoints()))
                .targetP(getTargetP())
                .targetQ(getTargetQ())
                .participate(getParticipate())
                .droop(getDroop())
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }

}
