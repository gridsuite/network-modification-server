/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.BatteryModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.BatteryModification;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
@ToString(callSuper = true)
@Schema(description = "Battery modification")
@JsonTypeName("BATTERY_MODIFICATION")
@ModificationErrorTypeName("MODIFY_BATTERY_ERROR")
public class BatteryModificationInfos extends InjectionModificationInfos {
    @Schema(description = "Minimum active power")
    private AttributeModification<Double> minActivePower;

    @Schema(description = "Maximum active power")
    private AttributeModification<Double> maxActivePower;

    @Schema(description = "Active power set point")
    private AttributeModification<Double> activePowerSetpoint;

    @Schema(description = "Reactive power set point")
    private AttributeModification<Double> reactivePowerSetpoint;

    @Schema(description = "Participate")
    private AttributeModification<Boolean> participate;

    @Schema(description = "Droop")
    private AttributeModification<Float> droop;

    @Schema(description = "Minimum reactive power")
    private AttributeModification<Double> minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private AttributeModification<Double> maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveModificationInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Reactive capability curve")
    private AttributeModification<Boolean> reactiveCapabilityCurve;

    @Override
    public BatteryModificationEntity toEntity() {
        return new BatteryModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new BatteryModification(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Battery modification ${batteryId}", "batteryId", this.getEquipmentId());
    }
}
