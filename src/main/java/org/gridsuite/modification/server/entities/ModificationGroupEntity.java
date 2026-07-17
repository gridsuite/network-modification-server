/*
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Entity
@DiscriminatorValue("GROUP")
@Table(name = "modificationGroup")
@PrimaryKeyJoinColumn(name = "id", foreignKey = @ForeignKey(name = "modification_group_container_fk"))
public class ModificationGroupEntity extends AbstractModificationContainerEntity {

    public ModificationGroupEntity(UUID uuid) {
        super(uuid, ModificationContainerType.GROUP);
    }
}
