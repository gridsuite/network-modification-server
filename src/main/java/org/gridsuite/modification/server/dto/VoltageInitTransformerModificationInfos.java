/*
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.ThreeWindingsTransformer;
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
@Schema(description = "Voltage init transformer modfication infos")
public class VoltageInitTransformerModificationInfos {
    @Schema(description = "Transformer id")
    private String transformerId;

    @Schema(description = "Ratio tap changer position")
    private Integer ratioTapChangerPosition;

    @Schema(description = "3 windings transformer leg side")
    private ThreeWindingsTransformer.Side legSide;
}
