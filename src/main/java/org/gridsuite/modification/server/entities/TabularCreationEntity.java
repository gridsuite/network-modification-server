/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OrderColumn;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.TabularCreationInfos;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tabular_creation")
public class TabularCreationEntity extends ModificationEntity {

    @Column(name = "creationType")
    @Enumerated(EnumType.STRING)
    private ModificationType creationType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn
    private List<ModificationEntity> creations;

    public TabularCreationEntity(@NonNull TabularCreationInfos tabularCreationInfos) {
        super(tabularCreationInfos);
        assignAttributes(tabularCreationInfos);
    }

    @Override
    public TabularCreationInfos toModificationInfos() {
        List<ModificationInfos> creationsInfos = creations.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
        return TabularCreationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .activated(getActivated())
                .creationType(creationType)
                .creations(creationsInfos)
                .build();
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((TabularCreationInfos) modificationInfos);
    }

    private void assignAttributes(TabularCreationInfos tabularCreationInfos) {
        creationType = tabularCreationInfos.getCreationType();
        if (creations == null) {
            creations = tabularCreationInfos.getCreations().stream()
                .map(ModificationEntity::fromDTO)
                .toList();
        } else {
            creations.clear();
            creations.addAll(tabularCreationInfos.getCreations().stream()
                .map(ModificationEntity::fromDTO)
                .toList());
        }
    }
}
