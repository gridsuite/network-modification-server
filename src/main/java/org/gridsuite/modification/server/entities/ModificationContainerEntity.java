/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * Container-identity supertable. One row per group/composite, giving {@code modification.container_id}
 * a single referential target across container types. Read model is unchanged: the discriminator still
 * lives on {@link ModificationEntity}.
 *
 * @author Hugo Marcellin {@literal <hugo.marcelin at rte-france.com>}
 */
@Getter
@NoArgsConstructor
@Entity
@Table(name = "modification_container")
public class ModificationContainerEntity extends AbstractManuallyAssignedIdentifierEntity<UUID> {

    @Id
    @Column(name = "id")
    private UUID id;

    @Column(name = "container_type")
    @Enumerated(EnumType.STRING)
    private ModificationContainerType containerType;

    public ModificationContainerEntity(UUID id, ModificationContainerType containerType) {
        this.id = id;
        this.containerType = containerType;
    }
}
