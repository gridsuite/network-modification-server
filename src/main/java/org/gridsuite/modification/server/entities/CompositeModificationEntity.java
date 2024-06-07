/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.dto.CompositeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "composite_modification")
public class CompositeModificationEntity extends ModificationEntity {

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn
    private List<ModificationEntity> modificationsList;

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modificationsList.stream().map(ModificationEntity::toModificationInfos).toList();
        return CompositeModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .modificationsList(modificationsInfos)
                .build();
    }

    private void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        if (modificationsList == null) {
            modificationsList = compositeModificationInfos.getModificationsList().stream().map(ModificationInfos::toEntity).toList();
        } else {
            modificationsList.clear();
            modificationsList.addAll(compositeModificationInfos.getModificationsList().stream().map(ModificationInfos::toEntity).toList());
        }
    }
}
