/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import jakarta.persistence.Column;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.PrimaryKeyJoinColumn;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.ColumnDefault;

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

    @Column(name = "name")
    @ColumnDefault("'My Composite'")
    private String name;

    public CompositeContainerEntity(UUID id, String name) {
        super(id, ModificationContainerType.COMPOSITE);
        this.name = name;
    }
}
