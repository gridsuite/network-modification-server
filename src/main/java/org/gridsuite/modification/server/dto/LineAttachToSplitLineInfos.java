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
public class LineAttachToSplitLineInfos extends ModificationInfos {

    @Schema(description = "line 1 id")
    private String lineToAttachTo1Id;

    @Schema(description = "line 2 id")
    private String lineToAttachTo2Id;

    @Schema(description = "attachment line id")
    private String attachedLineId;

    @Schema(description = "if no new voltage level, ID for the existing voltage level")
    private String existingVoltageLevelId;

    @Schema(description = "bus bar section or bus id")
    private String bbsOrBusId;

    @Schema(description = "new line 1 ID")
    private String newLine1Id;

    @Schema(description = "new line 1 name")
    private String newLine1Name;

    @Schema(description = "new line 1 ID")
    private String newLine2Id;

    @Schema(description = "new line 2 name")
    private String newLine2Name;
}
