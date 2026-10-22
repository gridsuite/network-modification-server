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
 * @param tags root network tags
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public record RootNetworkTagsCheckInfos(List<UUID> containerUuids, List<String> tags) {
    public RootNetworkTagsCheckInfos {
        containerUuids = Objects.requireNonNullElse(containerUuids, List.of());
        tags = Objects.requireNonNullElse(tags, List.of());
    }
}
