/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.EnergySource;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@ToString(callSuper = true)
@Schema(description = "generator modification")
public class GeneratorModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Energy source")
    private AttributeModification<EnergySource> energySource;

    @Schema(description = "Minimum active power")
    private AttributeModification<Double> minActivePower;

    @Schema(description = "Maximum active power")
    private AttributeModification<Double> maxActivePower;

    @Schema(description = "Rated nominal power")
    private AttributeModification<Double> ratedNominalPower;

    @Schema(description = "Active power set point")
    private AttributeModification<Double> activePowerSetpoint;

    @Schema(description = "Reactive power set point")
    private AttributeModification<Double> reactivePowerSetpoint;

    @Schema(description = "Voltage regulation on")
    private AttributeModification<Boolean> voltageRegulationOn;

    @Schema(description = "Voltage set point")
    private AttributeModification<Double> voltageSetpoint;
}
