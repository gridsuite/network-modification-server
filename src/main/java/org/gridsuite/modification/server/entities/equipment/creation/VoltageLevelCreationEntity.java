/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import com.powsybl.iidm.network.SwitchKind;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.CouplingDeviceInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelCreation_id_fk_constraint"))
public class VoltageLevelCreationEntity extends EquipmentCreationEntity {

    @Column
    private String substationId;

    @Column
    private double nominalV;

    @Column
    private Double lowVoltageLimit;

    @Column
    private Double highVoltageLimit;

    @Column
    private Double ipMin;

    @Column
    private Double ipMax;

    @Column
    private int busbarCount;

    @Column
    private int sectionCount;

    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable
    private List<SwitchKind> switchKinds;

    @ElementCollection
    @CollectionTable
    private List<CouplingDeviceCreationEmbeddable> couplingDevices;

    @OneToOne(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(
            name = "substation_creation_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "voltageLevel_substationCreation_fk"
            ))
    private SubstationCreationEntity substationCreation;

    public VoltageLevelCreationEntity(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        super(voltageLevelCreationInfos);
        assignAttributes(voltageLevelCreationInfos);
        this.setMessageType(getSubstationCreation() != null ? getType() + "_" + ModificationType.SUBSTATION_CREATION
                : getMessageType());
    }

    public static List<CouplingDeviceCreationEmbeddable> toEmbeddableCouplingDevices(List<CouplingDeviceInfos> couplingDevicesInfos) {
        return couplingDevicesInfos.stream()
                .map(couplingDevice -> new CouplingDeviceCreationEmbeddable(couplingDevice.getBusbarSectionId1(),
                        couplingDevice.getBusbarSectionId2()))
                .collect(Collectors.toList());
    }

    @Override
    public VoltageLevelCreationInfos toModificationInfos() {
        return toVoltageLevelCreationInfosBuilder().build();
    }

    public VoltageLevelCreationInfos toVoltageLevelCreationInfos() {
        return toVoltageLevelCreationInfosBuilder()
                .build();
    }

    private VoltageLevelCreationInfos.VoltageLevelCreationInfosBuilder<?, ?> toVoltageLevelCreationInfosBuilder() {
        SubstationCreationEntity substationCreationEntity = getSubstationCreation();
        List<CouplingDeviceInfos> couplingDeviceInfos = couplingDevices.stream()
                .map(cde -> new CouplingDeviceInfos(cde.getBusbarSectionId1(), cde.getBusbarSectionId2()))
                .collect(Collectors.toList());
        return VoltageLevelCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .substationId(getSubstationId())
                .nominalV(getNominalV())
                .lowVoltageLimit(getLowVoltageLimit())
                .highVoltageLimit(getHighVoltageLimit())
                .ipMin(getIpMin())
                .ipMax(getIpMax())
                .busbarCount(getBusbarCount())
                .sectionCount(getSectionCount())
                .switchKinds(getSwitchKinds())
                .couplingDevices(couplingDeviceInfos)
                .substationCreation(substationCreationEntity != null ? substationCreationEntity.toSubstationCreationInfos() : null)
                // properties
                .properties(CollectionUtils.isEmpty(getProperties()) ? null :
                        getProperties().stream()
                                .map(FreePropertyEntity::toInfos)
                                .toList());
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelCreationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        this.substationId = voltageLevelCreationInfos.getSubstationId();
        this.nominalV = voltageLevelCreationInfos.getNominalV();
        this.lowVoltageLimit = voltageLevelCreationInfos.getLowVoltageLimit();
        this.highVoltageLimit = voltageLevelCreationInfos.getHighVoltageLimit();
        this.ipMin = voltageLevelCreationInfos.getIpMin();
        this.ipMax = voltageLevelCreationInfos.getIpMax();
        this.busbarCount = voltageLevelCreationInfos.getBusbarCount();
        this.sectionCount = voltageLevelCreationInfos.getSectionCount();
        this.switchKinds = new ArrayList<>(voltageLevelCreationInfos.getSwitchKinds());
        this.couplingDevices = toEmbeddableCouplingDevices(voltageLevelCreationInfos.getCouplingDevices());
        this.substationCreation = Optional.ofNullable(voltageLevelCreationInfos.getSubstationCreation())
                .map(SubstationCreationEntity::new)
                .orElse(null);
    }
}

