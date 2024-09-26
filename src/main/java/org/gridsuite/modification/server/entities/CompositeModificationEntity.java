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

import java.util.ArrayList;
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
    @JoinTable(
            name = "compositeModificationSubModifications",
            joinColumns = @JoinColumn(name = "id"), foreignKey = @ForeignKey(name = "composite_modification_sub_modifications_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "modificationId"), inverseForeignKey = @ForeignKey(name = "modification_id_fk"))
    @OrderColumn
    private List<ModificationEntity> modifications = new ArrayList<>();

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream().map(ModificationEntity::toModificationInfos).toList();
        return CompositeModificationInfos.builder()
                .activated(getActivated())
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .modifications(modificationsInfos)
                .build();
    }

    // when we go back to an empty list, dont use addAll() on the list because JPA could start
    // @OrderColumn to 1 instead of 0
    private void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        modifications.clear();
        modifications = compositeModificationInfos.getModifications().stream().map(ModificationInfos::toEntity).toList();
    }
}
