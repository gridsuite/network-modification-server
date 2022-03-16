/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import javax.persistence.*;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

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
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    //TODO à bouger dans Modification
//    @JoinColumn(name = "groupId", foreignKey = @ForeignKey(name = "group_id_fk_constraint"), nullable = false)
    @Setter
    @OrderColumn
    private List<ModificationEntity> modifications = new ArrayList<>();

    public ModificationGroupEntity(UUID uuid) {
        this.id = uuid;
    }

    public void addModification(ModificationEntity modification) {
        modifications.add(modification);
        modification.setGroup(this);
    }

    public void removeComment(ModificationEntity modification) {
        modifications.remove(modification);
        modification.setGroup(null);
    }
}
