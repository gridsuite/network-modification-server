/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.*;

import java.util.List;
import java.util.stream.Collectors;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.*;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
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

    public TabularModificationEntity(@NonNull TabularModificationInfos tabularModificationInfos) {
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
                .modificationType(modificationType)
                .modifications(modificationsInfos)
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
            modifications = tabularModificationInfos.getModifications().stream().map(modificationInfos -> modificationInfos.toEntity()).toList();
        } else {
            modifications.clear();
            modifications.addAll(tabularModificationInfos.getModifications().stream().map(modificationInfos -> modificationInfos.toEntity()).toList());
        }
    }
}
