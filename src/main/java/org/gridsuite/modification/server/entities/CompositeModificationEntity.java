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
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.hibernate.annotations.ColumnDefault;

import java.util.ArrayList;
import java.util.Comparator;
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

    @Column(name = "name")
    @ColumnDefault("'My Composite'")
    private String name;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinTable(
            name = "compositeModificationSubModifications",
            joinColumns = @JoinColumn(name = "id"), foreignKey = @ForeignKey(name = "composite_modification_sub_modifications_id_fk"),
            inverseJoinColumns = @JoinColumn(name = "modificationId"), inverseForeignKey = @ForeignKey(name = "modification_id_fk"))
    @OrderBy("modificationsOrder asc")
    private List<ModificationEntity> modifications = new ArrayList<>();

    public CompositeModificationEntity(@NonNull CompositeModificationInfos compositeModificationInfos) {
        super(compositeModificationInfos);
        assignAttributes(compositeModificationInfos);
    }

    @Override
    public CompositeModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream()
                .sorted(Comparator.comparing(ModificationEntity::getModificationsOrder))
                .map(ModificationEntity::toModificationInfos).toList();
        return CompositeModificationInfos.builder()
                .name(getName())
                .activated(getActivated())
                .description(getDescription())
                .date(getDate())
                .uuid(getId())
                .stashed(getStashed())
                .modificationsInfos(modificationsInfos)
                .build();
    }

    private void assignAttributes(CompositeModificationInfos compositeModificationInfos) {
        this.setName(compositeModificationInfos.getName());
        modifications.clear();
        modifications.addAll(compositeModificationInfos.getModificationsInfos().stream()
                .map(ModificationEntity::fromDTO)
                .toList());

        for (int i = 0; i < modifications.size(); i++) {
            modifications.get(i).setModificationsOrder(i);
        }
    }
}
