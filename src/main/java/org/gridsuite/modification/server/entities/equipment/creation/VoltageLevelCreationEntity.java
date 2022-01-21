/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.Table;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.EquipmenModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
@Entity
@Table
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "voltageLevelCreation_id_fk_constraint"))
public class VoltageLevelCreationEntity extends EquipmentCreationEntity {

    @Column
    private double nominalVoltage;

    @Column
    private String substationId;

    @ElementCollection
    @CollectionTable
    private List<BusbarSectionCreationEmbeddable> busbarSections;

    @ElementCollection
    @CollectionTable
    private List<BusbarConnectionCreationEmbeddable> busbarConnections;

    public VoltageLevelCreationEntity(String equipmentId, String equipmentName, double nominalVoltage, String substationId,
        List<BusbarSectionCreationEmbeddable> busbarSections,
        List<BusbarConnectionCreationEmbeddable> busbarConnections) {
        super(ModificationType.VOLTAGE_LEVEL_CREATION,
                equipmentId,
                equipmentName);
        this.nominalVoltage = nominalVoltage;
        this.substationId = substationId;
        this.busbarSections = busbarSections;
        this.busbarConnections = busbarConnections;
    }

    @Override
    public VoltageLevelCreationInfos toModificationInfos() {
        return toVoltageLevelCreationInfosBuilder().build();
    }

    @Override
    public EquipmenModificationInfos toEquipmentModificationInfos(Set<String> uuids) {
        return toVoltageLevelCreationInfosBuilder().substationIds(uuids).build();
    }

    public VoltageLevelCreationInfos toVoltageLevelCreationInfos() {
        return toVoltageLevelCreationInfosBuilder().build();
    }

    private VoltageLevelCreationInfos.VoltageLevelCreationInfosBuilder<?, ?> toVoltageLevelCreationInfosBuilder() {
        List<BusbarSectionCreationInfos> bbsis = busbarSections.stream().map(bbbse ->
            new BusbarSectionCreationInfos(bbbse.getId(), bbbse.getName(), bbbse.getVertPos(), bbbse.getHorizPos())).collect(Collectors.toList());
        List<BusbarConnectionCreationInfos> cnxis = busbarConnections.stream().map(cnxe ->
            new BusbarConnectionCreationInfos(cnxe.getFromBBS(), cnxe.getToBBS(), cnxe.getSwitchKind())).collect(Collectors.toList());
        return VoltageLevelCreationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentName(getEquipmentName())
                .nominalVoltage(getNominalVoltage())
                .substationId(getSubstationId())
                .busbarSections(bbsis)
                .busbarConnections(cnxis);
    }
}

