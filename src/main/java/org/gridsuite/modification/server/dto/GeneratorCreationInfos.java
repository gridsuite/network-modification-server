/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.EnergySource;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import java.util.List;

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

    @Schema(description = "Marginal cost")
    private Double marginalCost;

    @Schema(description = "Minimum reactive power")
    private Double minimumReactivePower;

    @Schema(description = "Maximum reactive power")
    private Double maximumReactivePower;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveCreationInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Participate")
    private Boolean participate;

    @Schema(description = "Droop")
    private Float droop;

    @Schema(description = "Transient reactance")
    private Double transientReactance;

    @Schema(description = "Step up transformer reactance")
    private Double stepUpTransformerReactance;

    @Schema(description = "Regulating terminal equipment id")
    private String regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private String regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private String regulatingTerminalVlId;

    @Schema(name = "Remote reactive power control enabled")
    private Boolean remoteReactivePowerControlEnabled;

    @Schema(name = "Q percent")
    private Double qPercent;

    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Schema(description = "Connection Name")
    private String connectionName;

    @Schema(description = "Connection Direction")
    private ConnectablePosition.Direction connectionDirection;
}
