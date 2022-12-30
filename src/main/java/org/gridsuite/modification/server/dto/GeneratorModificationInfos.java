/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.GeneratorModification;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
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

    @Override
    public GeneratorModificationEntity toEntity() {
        return new GeneratorModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new GeneratorModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFY_GENERATOR_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.GENERATOR_MODIFICATION.name(), "Generator modification ${generatorId}", "generatorId", this.getEquipmentId());
    }
}
