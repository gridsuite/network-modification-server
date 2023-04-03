/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * This is the return value of the endpoint updateModificationGroup.
 * TODO : remove this DTO and return only networkModificationResult when missingModifications will not be needed anymore
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@Builder
@Getter
@Schema(description = "Copy or move network modifications result")
public class CopyOrMoveModificationResult {

    @Builder.Default
    Optional<NetworkModificationResult> networkModificationResult = Optional.empty();

    @Schema(description = "Network modification failures")
    @Builder.Default
    private List<UUID> missingModifications = List.of();
}
