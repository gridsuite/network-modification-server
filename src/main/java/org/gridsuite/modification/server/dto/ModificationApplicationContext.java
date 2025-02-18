/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import java.util.Collections;
import java.util.Set;
import java.util.UUID;

public record ModificationApplicationContext(UUID networkUuid, String variantId, UUID reportUuid, UUID reporterId, Set<UUID> excludedModifications) {
    public ModificationApplicationContext(UUID networkUuid, String variantId, UUID reportUuid, UUID reporterId) {
        this(networkUuid, variantId, reportUuid, reporterId, Collections.emptySet());
    }
}
