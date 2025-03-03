/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Builder
@Schema(description = "Build infos")
public class BuildInfos {
    private String originVariantId;

    private String destinationVariantId;

    private List<UUID> modificationGroupUuids = new ArrayList<>();

    // map with modification groups as key, modification to excludes as value
    @Builder.Default
    private Map<UUID, Set<UUID>> modificationUuidsToExclude = new HashMap<>();

    private List<ReportInfos> reportsInfos = new ArrayList<>();

}
