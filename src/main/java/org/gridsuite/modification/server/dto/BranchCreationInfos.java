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
@Schema(description = "Branch creation")
public class BranchCreationInfos extends EquipmentCreationInfos {

    @Schema(description = "Series resistance")
    private double r;

    @Schema(description = "Series reactance")
    private double x;

    @Schema(description = "Voltage level id Side 1")
    private String voltageLevelId1;

    @Schema(description = "Voltage level id Side 2")
    private String voltageLevelId2;

    @Schema(description = "Bus or Busbar section id Side 1")
    private String busOrBusbarSectionId1;

    @Schema(description = "Bus or Busbar section id Side 2")
    private String busOrBusbarSectionId2;

    @Schema(description = "Current limits Side 1")
    private CurrentLimitsInfos currentLimits1;

    @Schema(description = "Current limits Side 2")
    private CurrentLimitsInfos currentLimits2;

    @Schema(description = "Connectable position Side 1 (for substation diagram)")
    private ConnectablePositionInfos position1;

    @Schema(description = "Connectable position Side 2 (for substation diagram)")
    private ConnectablePositionInfos position2;
}
