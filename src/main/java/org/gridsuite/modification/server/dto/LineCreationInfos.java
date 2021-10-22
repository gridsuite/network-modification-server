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

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line creation")
public class LineCreationInfos extends BranchCreationInfos {

    @Schema(description = "Shunt conductance Side 1")
    private Double shuntConductance1;

    @Schema(description = "Shunt susceptance Side 1")
    private Double shuntSusceptance1;

    @Schema(description = "Shunt conductance Side 2")
    private Double shuntConductance2;

    @Schema(description = "Shunt susceptance Side 2")
    private Double shuntSusceptance2;

}
