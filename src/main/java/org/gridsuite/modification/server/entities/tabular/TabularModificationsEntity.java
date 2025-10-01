/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.tabular;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.tabular.*;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "tabular_modifications")
public class TabularModificationsEntity extends ModificationEntity {

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "tabular_modification_id")
    @OrderColumn(name = "insert_position")
    private List<TabularPropertyEntity> properties;

    private String csvFilename;

    @Enumerated(EnumType.STRING)
    private ModificationType modificationType;

    @OneToMany(cascade = CascadeType.ALL)
    @OrderColumn
    private List<ModificationEntity> modifications;

    public TabularModificationsEntity(@NonNull TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        assignModificationAttributes(tabularModificationInfos);
    }

    public TabularModificationsEntity(@NonNull LimitSetsTabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        assignModificationAttributes(tabularModificationInfos);
    }

    public TabularModificationsEntity(@NonNull TabularCreationInfos tabularCreationInfos) {
        super(tabularCreationInfos);
        assignCreationAttributes(tabularCreationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        if (modificationInfos.getType() == ModificationType.TABULAR_CREATION) {
            assignCreationAttributes((TabularCreationInfos) modificationInfos);
        } else {
            assignModificationAttributes((TabularModificationInfos) modificationInfos);
        }
    }

    @Override
    public TabularBaseInfos toModificationInfos() {
        //throw new UnsupportedOperationException("DTO from tabular entity is build with optimized loadTabularModification()");
        // TODO DBR: should have no IF with a single DTO
        if (modificationType == ModificationType.TABULAR_CREATION) {
            List<ModificationInfos> creationsInfos = modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
            return TabularCreationInfos.builder()
                    .date(getDate())
                    .uuid(getId())
                    .stashed(getStashed())
                    .activated(getActivated())
                    .creationType(modificationType)
                    .creations(creationsInfos)
                    .properties(CollectionUtils.isEmpty(getProperties()) ? null : getProperties().stream()
                            .map(TabularPropertyEntity::toInfos)
                            .toList())
                    .csvFilename(getCsvFilename())
                    .build();
        } else {
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
                    .csvFilename(getCsvFilename())
                    .build();
        }
    }

    // TODO DBR: should have a single assign function with a single DTO
    private void assignCommonAttributes(TabularBaseInfos tabularBaseInfos) {
        List<TabularPropertyEntity> newProperties = tabularBaseInfos.getProperties() == null ? null :
                tabularBaseInfos.getProperties().stream()
                        .map(TabularPropertyEntity::new)
                        .toList();
        if (this.properties != null) {
            this.properties.clear();
            if (newProperties != null) {
                this.properties.addAll(newProperties);
            }
        } else {
            this.properties = newProperties;
        }
        this.csvFilename = tabularBaseInfos.getCsvFilename();
    }

    private void assignModificationAttributes(TabularModificationInfos tabularModificationInfos) {
        assignCommonAttributes(tabularModificationInfos);
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
    }

    private void assignCreationAttributes(TabularCreationInfos tabularCreationInfos) {
        assignCommonAttributes(tabularCreationInfos);
        modificationType = tabularCreationInfos.getCreationType();
        if (modifications == null) {
            modifications = tabularCreationInfos.getCreations().stream()
                    .map(ModificationEntity::fromDTO)
                    .toList();
        } else {
            modifications.clear();
            modifications.addAll(tabularCreationInfos.getCreations().stream()
                    .map(ModificationEntity::fromDTO)
                    .toList());
        }
    }
}
