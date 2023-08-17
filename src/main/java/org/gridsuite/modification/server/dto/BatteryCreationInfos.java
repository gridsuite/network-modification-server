/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.BatteryCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.BatteryCreation;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Battery creation")
@JsonTypeName("BATTERY_CREATION")
@ModificationErrorTypeName("CREATE_BATTERY_ERROR")
public class BatteryCreationInfos extends InjectionCreationInfos {

    @Schema(description = "Minimum active power")
    private double minActivePower;

    @Schema(description = "Maximum active power")
    private double maxActivePower;

    @Schema(description = "Minimum reactive power")
    private Double minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private Double maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveCreationInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Active power set point")
    private double activePowerSetpoint;

    @Schema(description = "Reactive power set point")
    private Double reactivePowerSetpoint;

    @Schema(description = "Participate")
    private Boolean participate;

    @Schema(description = "Droop")
    private Float droop;

    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Schema(description = "Connection Name")
    private String connectionName;

    @Schema(description = "Connection Direction")
    @Enumerated(EnumType.ORDINAL)
    private ConnectablePosition.Direction connectionDirection;

    @Schema(description = "Connection Position")
    private Integer connectionPosition;

    @Override
    public BatteryCreationEntity toEntity() {
        return new BatteryCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new BatteryCreation(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_BATTERY_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.BATTERY_CREATION.name(), "Battery creation ${batteryId}", "batteryId", this.getEquipmentId());
    }
}
