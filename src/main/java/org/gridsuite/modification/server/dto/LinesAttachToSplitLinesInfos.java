/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;


/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line attach to split line")
public class LinesAttachToSplitLinesInfos extends ModificationInfos {

    @Schema(description = "line 1 id")
    private String lineToAttachTo1Id;

    @Schema(description = "line 2 id")
    private String lineToAttachTo2Id;

    @Schema(description = "attachment line id")
    private String attachedLineId;

    @Schema(description = "ID for the existing voltage level")
    private String voltageLevelId;

    @Schema(description = "bus bar section or bus id")
    private String bbsBusId;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine1Id;

    @Schema(description = "replacing line 1 name")
    private String replacingLine1Name;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine2Id;

    @Schema(description = "replacing line 2 name")
    private String replacingLine2Name;
}
