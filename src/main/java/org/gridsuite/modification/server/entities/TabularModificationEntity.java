/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;

import java.util.List;
import java.util.stream.Collectors;

import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.springframework.util.CollectionUtils;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tabular_modification")
public class TabularModificationEntity extends ModificationEntity {

    @Column(name = "modificationType")
    @Enumerated(EnumType.STRING)
    private ModificationType modificationType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn
    private List<ModificationEntity> modifications;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "tabular_modification_id")
    @OrderColumn(name = "insert_position")
    private List<TabularPropertyEntity> properties;

    public TabularModificationEntity(@NonNull TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        assignAttributes(tabularModificationInfos);
    }

    public TabularModificationEntity(@NonNull LimitSetsTabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        assignAttributes(tabularModificationInfos);
    }

    @Override
    public TabularModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
        return TabularModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .activated(getActivated())
                .modificationType(modificationType)
                .modifications(modificationsInfos)
                .properties(CollectionUtils.isEmpty(getProperties()) ? null : getProperties().stream()
                        .map(TabularPropertyEntity::toInfos)
                        .toList())
                .build();
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((TabularModificationInfos) modificationInfos);
    }

    private void assignAttributes(TabularModificationInfos tabularModificationInfos) {
        modificationType = tabularModificationInfos.getModificationType();
        if (modifications == null) {
            modifications = tabularModificationInfos.getModifications().stream()
                .map(ModificationEntity::fromDTO)
                .collect(Collectors.toList());
        } else {
            modifications.clear();
            modifications.addAll(tabularModificationInfos.getModifications().stream()
                .map(ModificationEntity::fromDTO)
                .toList());
        }
        List<TabularPropertyEntity> newProperties = tabularModificationInfos.getProperties() == null ? null :
                tabularModificationInfos.getProperties().stream()
                        .map(TabularPropertyEntity::new)
                        .toList();
        if (this.properties != null) {
            // update using the same reference with clear/add (to avoid JPA exception)
            this.properties.clear();
            if (newProperties != null) {
                this.properties.addAll(newProperties);
            }
        } else {
            this.properties = newProperties;
        }
    }
}
