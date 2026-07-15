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
import lombok.Setter;

import java.util.UUID;

/**
 * @author Hugo Marcellin {@literal <hugo.marcelin at rte-france.com>}
 */
@Getter
@Setter
@NoArgsConstructor
@Entity
@DiscriminatorValue("COMPOSITE")
@Table(name = "composite_container")
@PrimaryKeyJoinColumn(name = "id", foreignKey = @ForeignKey(name = "composite_container_container_fk"))
public class CompositeContainerEntity extends AbstractModificationContainerEntity {

    public CompositeContainerEntity(UUID id) {
        super(id, ModificationContainerType.COMPOSITE);
    }
}
