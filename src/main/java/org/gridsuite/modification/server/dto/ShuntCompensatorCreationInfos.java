/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
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

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Shunt compensator creation")
public class ShuntCompensatorCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Maximum number of sections")
    private Integer maximumNumberOfSections;

    @Schema(description = "Current number of sections")
    private Integer currentNumberOfSections;

    @Schema(description = "Susceptance per section")
    private Double susceptancePerSection;

    @Schema(description = "Identical sections")
    private Boolean isIdenticalSection;
}
