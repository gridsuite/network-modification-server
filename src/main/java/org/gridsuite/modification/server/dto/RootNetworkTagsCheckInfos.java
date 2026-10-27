/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

/**
 * Modifications to check against root network tags.
 *
 * @param containerUuids groups or composites: only the shared modifications their content refers to are checked
 * @param modificationUuids modifications: only the shared modifications they and their content refer to are checked
 * @param sharedUuids shared modifications: checked themselves, along with the shared modifications their content refers to
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public record RootNetworkTagsCheckInfos(List<UUID> containerUuids, List<UUID> modificationUuids, List<UUID> sharedUuids,
                                        List<String> tags) {
    public RootNetworkTagsCheckInfos {
        containerUuids = Objects.requireNonNullElse(containerUuids, List.of());
        modificationUuids = Objects.requireNonNullElse(modificationUuids, List.of());
        sharedUuids = Objects.requireNonNullElse(sharedUuids, List.of());
        tags = Objects.requireNonNullElse(tags, List.of());
    }
}
