/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Schema(description = "Report infos")
public class ReportInfos {
    private UUID reportUuid;

    private UUID nodeUuid;

    private ReportMode reportMode;

    public ReportInfos(UUID reportUuid, UUID nodeUuid) {
        this.reportUuid = reportUuid;
        this.nodeUuid = nodeUuid;
        this.reportMode = ReportMode.APPEND;
    }
}
