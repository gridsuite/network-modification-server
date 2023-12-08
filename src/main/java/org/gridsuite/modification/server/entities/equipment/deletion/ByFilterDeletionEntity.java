/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "byFilterDeletion")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "ByFilterDeletion_id_fk_constraint"))
public class ByFilterDeletionEntity extends ModificationEntity {
    @Column(name = "equipmentType")
    private String equipmentType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> equipmentFilters;

    public ByFilterDeletionEntity(ByFilterDeletionInfos byFilterDeletionInfos) {
        super(byFilterDeletionInfos);
        assignAttributes(byFilterDeletionInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ByFilterDeletionInfos) modificationInfos);
    }

    private void assignAttributes(ByFilterDeletionInfos byFilterDeletionInfos) {
        this.equipmentType = byFilterDeletionInfos.getEquipmentType();
        if (equipmentFilters == null) {
            this.equipmentFilters = byFilterDeletionInfos.getEquipmentFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList());
        } else {
            equipmentFilters.clear();
            equipmentFilters.addAll(byFilterDeletionInfos.getEquipmentFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList()));
        }
    }

    @Override
    public ByFilterDeletionInfos toModificationInfos() {
        return ByFilterDeletionInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .equipmentFilters(this.getEquipmentFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId(), filter.getName()))
                        .collect(Collectors.toList()))
                .equipmentType(getEquipmentType()).build();
    }
}
