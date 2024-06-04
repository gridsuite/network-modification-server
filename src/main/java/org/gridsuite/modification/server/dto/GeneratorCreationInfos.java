/**
 * Copyright (c) 2020, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.EnergySource;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;

import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.GeneratorCreation;

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
@JsonTypeName("GENERATOR_CREATION")
@ModificationErrorTypeName("CREATE_GENERATOR_ERROR")
public class GeneratorCreationInfos extends InjectionCreationInfos implements ReactiveLimitsHolderInfos {
    @Schema(description = "Energy source")
    private EnergySource energySource;

    @Schema(description = "Minimum active power")
    private double minP;

    @Schema(description = "Maximum active power")
    private double maxP;

    @Schema(description = "Rated nominal power")
    private Double ratedS;

    @Schema(description = "Active power set point")
    private double targetP;

    @Schema(description = "Reactive power set point")
    private Double targetQ;

    @Schema(description = "Voltage regulation on")
    private boolean voltageRegulationOn;

    @Schema(description = "Voltage set point")
    private Double targetV;

    @Schema(description = "Planning active power set point")
    private Double plannedActivePowerSetPoint;

    @Schema(description = "Marginal cost")
    private Double marginalCost;

    @Schema(description = "Planning outage rate")
    private Double plannedOutageRate;

    @Schema(description = "Forced outage rate")
    private Double forcedOutageRate;

    @Schema(description = "Minimum reactive power")
    private Double minQ;

    @Schema(description = "Maximum reactive power")
    private Double maxQ;

    @Schema(description = "Reactive capability curve points")
    private List<ReactiveCapabilityCurveCreationInfos> reactiveCapabilityCurvePoints;

    @Schema(description = "Participate")
    private Boolean participate;

    @Schema(description = "Droop")
    private Float droop;

    @Schema(description = "Transient reactance")
    private Double directTransX;

    @Schema(description = "Step up transformer reactance")
    private Double stepUpTransformerX;

    @Schema(description = "Regulating terminal equipment id")
    private String regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private String regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private String regulatingTerminalVlId;

    // As this attribute has only one lower case letter at its start (xXXXX), the getters is parsed as getQPercent and the field for Jackson is parsed as qpercent
    // while we expect qPercent. JsonProperty let fix the json field to qPercent
    @JsonProperty("qPercent")
    @Schema(description = "Q percent")
    private Double qPercent;

    @Schema(description = "Reactive capability curve")
    private Boolean reactiveCapabilityCurve;

    @Override
    public GeneratorCreationEntity toEntity() {
        return new GeneratorCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new GeneratorCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Generator creation ${equipmentId}")
                .withUntypedValue("equipmentId", this.getEquipmentId())
                .add();
    }
}
