/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import java.util.List;
import java.util.UUID;

/**
 * @author Hugo Marcellin {@literal <hugo.marcelin at rte-france.com>}
 */
public interface ModificationContainer {

    UUID getId();

    ModificationContainerType getContainerType();

    List<ModificationEntity> getModifications();

    void addModification(ModificationEntity child, int position);

    void setModifications(List<ModificationEntity> newChildren);

    default void reattachDescendants() {
        UUID parentId = getId();
        ModificationContainerType parentType = getContainerType();
        for (ModificationEntity child : getModifications()) {
            if (child == null) {
                continue;
            }
            if (!parentId.equals(child.getContainerId()) || parentType != child.getContainerType()) {
                child.attachToContainer(this);
            }
            if (child instanceof ModificationContainer nested) {
                nested.reattachDescendants();
            }
        }
    }
}
