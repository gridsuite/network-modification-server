/*
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
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@Schema(description = "Voltage init shunt compensator modification infos")
public class VoltageInitShuntCompensatorModificationInfos {
    @Schema(description = "Shunt compensator id")
    private String shuntCompensatorId;

    @Schema(description = "Section count")
    private Integer sectionCount;

    @Schema(description = "Connexion")
    private Boolean connect;

    @Schema(description = "Target voltage")
    private Double targetV;
}
