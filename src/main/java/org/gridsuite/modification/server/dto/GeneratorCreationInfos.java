/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.EnergySource;
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
@Schema(description = "Generator creation")
public class GeneratorCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Energy source")
    private EnergySource energySource;

    @Schema(description = "Minimum active power")
    private double minActivePower;

    @Schema(description = "Maximum active power")
    private double maxActivePower;

    @Schema(description = "Rated nominal power")
    private Double ratedNominalPower;

    @Schema(description = "Active power set point")
    private double activePowerSetpoint;

    @Schema(description = "Reactive power set point")
    private Double reactivePowerSetpoint;

    @Schema(description = "Voltage regulation on")
    private boolean voltageRegulationOn;

    @Schema(description = "Voltage set point")
    private Double voltageSetpoint;
}
