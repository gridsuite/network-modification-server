/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import java.util.Iterator;
import java.util.List;
import java.util.UUID;

/**
 * Shared insert/remove logic used by both container implementations. Kept package-private
 * here rather than in a separate helper class to keep the diff small.
 *
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
final class ContainerOps {
    private ContainerOps() { }

    static void insert(ModificationContainer owner, List<ModificationEntity> list,
                       ModificationEntity child, int position) {
        int target = Math.max(0, Math.min(position, list.size()));
        child.attachToContainer(owner);
        list.add(target, child);
        renumber(list);
    }

    static boolean removeById(List<ModificationEntity> list, UUID childId) {
        Iterator<ModificationEntity> it = list.iterator();
        while (it.hasNext()) {
            ModificationEntity m = it.next();
            if (childId.equals(m.getId())) {
                it.remove();
                m.detachFromContainer();
                renumber(list);
                return true;
            }
        }
        return false;
    }

    private static void renumber(List<ModificationEntity> list) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).setModificationsOrder(i);
        }
    }
}
