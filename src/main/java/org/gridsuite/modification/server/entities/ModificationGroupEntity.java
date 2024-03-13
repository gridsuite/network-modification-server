/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "modificationGroup")
public class ModificationGroupEntity extends AbstractManuallyAssignedIdentifierEntity<UUID> {
    @Id
    @Column(name = "id")
    private UUID id;

    @OneToMany(
            mappedBy = "group",
            cascade = CascadeType.ALL
    )
    @OrderColumn
    private List<ModificationEntity> modifications = new ArrayList<>();

    public ModificationGroupEntity(UUID uuid) {
        this.id = uuid;
    }

    public void addModification(ModificationEntity modification) {
        if (modifications.isEmpty()) {
            List<ModificationEntity> newList = new ArrayList<>();
            newList.add(modification);
            setModifications(newList);
        } else {
            modifications.add(modification);
            modification.setGroup(this);
        }
    }

    public void setModifications(List<ModificationEntity> modifications) {
        this.modifications = modifications;
        modifications.forEach(modification ->
            modification.setGroup(this)
        );
    }
}
