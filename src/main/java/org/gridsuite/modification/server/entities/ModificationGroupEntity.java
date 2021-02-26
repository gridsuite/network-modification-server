/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.LinkedList;
import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "modificationGroup")
public class ModificationGroupEntity {
    @Id
    @Column(name = "id")
    private UUID uuid;

    @OneToMany(fetch = FetchType.EAGER, orphanRemoval = true, cascade = {CascadeType.ALL})
    @JoinColumn(name = "groupUuid")
    private List<AbstractModificationEntity> modifications;

    public ModificationGroupEntity(UUID uuid) {
        this.uuid = uuid;
        this.modifications = new LinkedList<>();
    }
}
