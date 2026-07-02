/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import java.util.List;

/**
 * Shared insert/renumber logic used by every container implementation.
 *
 * @author Hugo Marcellin {@literal <hugo.marcelin at rte-france.com>}
 */
final class ContainerOps {
    private ContainerOps() { }

    static void insert(ModificationContainer owner, List<ModificationEntity> list,
                       ModificationEntity child, int position) {
        int target = Math.clamp(position, 0, list.size());
        child.attachToContainer(owner);
        list.add(target, child);
        renumber(list);
    }

    private static void renumber(List<ModificationEntity> list) {
        for (int i = 0; i < list.size(); i++) {
            list.get(i).setModificationsOrder(i);
        }
    }
}
