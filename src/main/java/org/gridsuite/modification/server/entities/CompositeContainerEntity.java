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
import org.gridsuite.modification.NetworkModificationException;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.MOVE_MODIFICATION_ERROR;

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

    @Override
    public void insertModifications(List<ModificationEntity> childrenToInsert, UUID beforeModificationUuid) {
        assertNoCycle(childrenToInsert);
        super.insertModifications(childrenToInsert, beforeModificationUuid);
    }

    // TODO : replace by a recursive query to avoid N+1 problem
    private void assertNoCycle(List<ModificationEntity> childrenToInsert) {
        for (ModificationEntity m : childrenToInsert) {
            if (m instanceof CompositeModificationEntity childToInsert
                    && childToInsert.getContent().containsOrIsContainer(getId())) {
                throw new NetworkModificationException(MOVE_MODIFICATION_ERROR,
                        String.format("Moving composite (%s) into (%s) would create a cycle", m.getId(), getId()));
            }
        }
    }

    private boolean containsOrIsContainer(UUID targetId) {
        return getId().equals(targetId)
            || getModifications().stream().anyMatch(sub -> isOrContains(sub, targetId));
    }

    private boolean isOrContains(ModificationEntity sub, UUID targetId) {
        if (sub.getId().equals(targetId)) {
            return true;
        }
        return sub instanceof CompositeModificationEntity composite
            && composite.getContent().containsOrIsContainer(targetId);
    }
}
