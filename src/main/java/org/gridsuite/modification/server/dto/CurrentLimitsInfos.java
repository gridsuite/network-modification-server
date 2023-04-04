/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentLimitsEntity;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentTemporaryLimitCreationEmbeddable;

import java.util.List;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Current Limits")
public class CurrentLimitsInfos {

    @Schema(description = "Permanent current limit")
    private Double permanentLimit;

    @Schema(description = "Temporary current limits")
    private List<CurrentTemporaryLimitCreationInfos> temporaryLimits;

    public CurrentLimitsEntity toEntity() {
        return new CurrentLimitsEntity(null, permanentLimit, CurrentTemporaryLimitCreationEmbeddable.toEmbeddableCurrentTemporaryLimits(temporaryLimits));
    }
}
