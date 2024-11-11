/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.deletion;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;
import org.gridsuite.modification.server.mapper.MappingUtil;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "byFilterDeletion")
@PrimaryKeyJoinColumn(foreignKey = @ForeignKey(name = "by_filter_deletion_id_fk_constraint"))
public class ByFilterDeletionEntity extends ModificationEntity {
    @Enumerated(EnumType.STRING)
    @Column(name = "equipmentType")
    private IdentifiableType equipmentType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinTable(
            name = "byFilterDeletionFilters",
            joinColumns = @JoinColumn(name = "id"), foreignKey = @ForeignKey(name = "by_filter_deletion_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "filterId"), inverseForeignKey = @ForeignKey(name = "variation_filter_id_fk"))
    private List<VariationFilterEntity> filters;

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
        if (filters == null) {
            this.filters = byFilterDeletionInfos.getFilters().stream()
                .map(filter -> MappingUtil.<FilterInfos, VariationFilterEntity>mapToEntity(filter))
                .collect(Collectors.toList());
        } else {
            filters.clear();
            filters.addAll(byFilterDeletionInfos.getFilters().stream()
                .map(filter -> MappingUtil.<FilterInfos, VariationFilterEntity>mapToEntity(filter))
                .collect(Collectors.toList()));
        }
    }

    @Override
    public ByFilterDeletionInfos toModificationInfos() {
        return ByFilterDeletionInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .filters(getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId(), filter.getName()))
                        .collect(Collectors.toList()))
                .equipmentType(getEquipmentType()).build();
    }
}
