/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentLimitsModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentTemporaryLimitModificationEmbeddable;

import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Data
@Schema(description = "Current Limits")
public class CurrentLimitsModificationInfos {
    @Schema(description = "Permanent current limit")
    private Double permanentLimit;

    @Schema(description = "Temporary current limits")
    private List<CurrentTemporaryLimitModificationInfos> temporaryLimits;

    public CurrentLimitsModificationEntity toEntity() {
        return new CurrentLimitsModificationEntity(null, permanentLimit, CurrentTemporaryLimitModificationEmbeddable.toEmbeddableCurrentTemporaryLimits(temporaryLimits));
    }
}
