/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.BusbarSectionCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;

import javax.persistence.*;
import java.util.List;
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
    private double nominalVoltage;

    @Column
    private String substationId;

    @ElementCollection
    @CollectionTable
    private List<BusbarSectionCreationEmbeddable> busbarSections;

    @ElementCollection
    @CollectionTable
    private List<BusbarConnectionCreationEmbeddable> busbarConnections;

    public VoltageLevelCreationEntity(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        super(voltageLevelCreationInfos);
        assignAttributes(voltageLevelCreationInfos);
    }

    public static List<BusbarConnectionCreationEmbeddable> toEmbeddableConnections(
        List<BusbarConnectionCreationInfos> busbarConnectionsInfos) {
        return busbarConnectionsInfos == null ? List.of() : busbarConnectionsInfos.stream().map(cnxi ->
            new BusbarConnectionCreationEmbeddable(cnxi.getFromBBS(), cnxi.getToBBS(), cnxi.getSwitchKind())
        ).collect(Collectors.toList());
    }

    public static List<BusbarSectionCreationEmbeddable> toEmbeddableSections(List<BusbarSectionCreationInfos> busbarSectionsInfos) {
        return busbarSectionsInfos.stream()
            .map(bbsi ->
                new BusbarSectionCreationEmbeddable(bbsi.getId(), bbsi.getName(), bbsi.getVertPos(), bbsi.getHorizPos())
            ).collect(Collectors.toList());
    }

    @Override
    public VoltageLevelCreationInfos toModificationInfos() {
        return toVoltageLevelCreationInfosBuilder().build();
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

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((VoltageLevelCreationInfos) modificationInfos);
    }

    private void assignAttributes(VoltageLevelCreationInfos voltageLevelCreationInfos) {
        this.nominalVoltage = voltageLevelCreationInfos.getNominalVoltage();
        this.substationId = voltageLevelCreationInfos.getSubstationId();
        List<BusbarSectionCreationEmbeddable> busBarSections = toEmbeddableSections(voltageLevelCreationInfos.getBusbarSections());
        List<BusbarConnectionCreationEmbeddable> busBarConnections = toEmbeddableConnections(voltageLevelCreationInfos.getBusbarConnections());
        this.busbarSections = busBarSections;
        this.busbarConnections = busBarConnections;
    }
}

