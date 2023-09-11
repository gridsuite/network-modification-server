/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Converter station creation")
public class ConverterStationCreationInfos extends InjectionWithReactiveLimitsCreationInfos {
    @Schema(description = "Loss Factor")
    private Float lossFactor;

    @Schema(description = "Reactive power")
    private Double reactivePower;

    @Schema(description = "Voltage regulation")
    private Boolean voltageRegulationOn;

    @Schema(description = "Voltage")
    private Double voltage;
}
