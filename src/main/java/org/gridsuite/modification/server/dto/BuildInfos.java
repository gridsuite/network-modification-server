/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Build infos")
public class BuildInfos {
    private String originVariantId;

    private String destinationVariantId;

    private UUID reportUuid;

    private List<UUID> modificationGroupUuids = new ArrayList<>();

    private List<String> reporterIds = new ArrayList<>();

    private Set<UUID> modificationsToExclude = new HashSet<>();

    public void addModificationToExclude(UUID modificationUuid) {
        modificationsToExclude.add(modificationUuid);
    }
}
