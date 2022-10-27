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
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Two windings transformer creation")
public class TwoWindingsTransformerCreationInfos extends BranchCreationInfos {

    @Schema(description = "Magnetizing conductance")
    private double magnetizingConductance;

    @Schema(description = "Magnetizing susceptance")
    private double magnetizingSusceptance;

    @Schema(description = "side 1 rated voltage")
    private double ratedVoltage1;

    @Schema(description = "side 2 rated voltage")
    private double ratedVoltage2;

    @Schema(description = "Rated conductance in Siemens")
    private Double ratedS;

    @Schema(description = "Ratio tap changer")
    private RatioTapChangerCreationInfos ratioTapChanger;

    @Schema(description = "Phase tap changer")
    private PhaseTapChangerCreationInfos phaseTapChanger;

}
