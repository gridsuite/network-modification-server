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

import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;

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
    private String modificationType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
    @OrderColumn
    private List<ModificationEntity> modifications;

    public TabularModificationEntity(TabularModificationInfos tabularModificationInfos) {
        super(tabularModificationInfos);
        modificationType = tabularModificationInfos.getModificationType();
        switch (modificationType) {
            case "GENERATOR_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(generatorModificationInfos -> new GeneratorModificationEntity((GeneratorModificationInfos) generatorModificationInfos)).collect(Collectors.toList());
                break;
            case "LOAD_MODIFICATION":
                modifications = tabularModificationInfos.getModifications().stream().map(loadModificationInfos -> new LoadModificationEntity((LoadModificationInfos) loadModificationInfos)).collect(Collectors.toList());
                break;
            default:
                break;
        }
    }

    @Override
    public TabularModificationInfos toModificationInfos() {
        List<ModificationInfos> modificationsInfos = modifications.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
        return TabularModificationInfos.builder()
                .date(getDate())
                .uuid(getId())
                .modificationType(modificationType)
                .modifications(modificationsInfos)
                .build();
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        TabularModificationInfos tabularModificationInfos = (TabularModificationInfos) modificationInfos;
        modificationType = tabularModificationInfos.getModificationType();
        modifications.clear();
        switch (modificationType) {
            case "GENERATOR_MODIFICATION":
                modifications.addAll(tabularModificationInfos.getModifications().stream().map(generatorModificationInfos -> new GeneratorModificationEntity((GeneratorModificationInfos) generatorModificationInfos)).collect(Collectors.toList()));
                break;
            case "LOAD_MODIFICATION":
                modifications.addAll(tabularModificationInfos.getModifications().stream().map(loadModificationInfos -> new LoadModificationEntity((LoadModificationInfos) loadModificationInfos)).collect(Collectors.toList()));
                break;
            default:
                break;
        }
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
