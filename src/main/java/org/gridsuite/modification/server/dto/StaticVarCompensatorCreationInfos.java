/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.StaticVarCompensator;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.StaticCompensatorCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.StaticVarCompensatorCreation;

/**
 * @author Ghazwa Rehili <ghazwa.rehili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Static var compensator creation")
@JsonTypeName("STATIC_VAR_COMPENSATOR_CREATION")
@ModificationErrorTypeName("CREATE_STATIC_VAR_COMPENSATOR_ERROR")
public class StaticVarCompensatorCreationInfos extends InjectionCreationInfos {
    @Schema(description = "Maximal susceptance available")
    private Double maxSusceptance;

    @Schema(description = "Minimum susceptance available")
    private Double minSusceptance;

    @Schema(description = "Maximal qAtNominalV available")
    private Double maxQAtNominalV;

    @Schema(description = "Minimum qAtNominalV available")
    private Double minQAtNominalV;

    @Schema(description = "regulation mode")
    private StaticVarCompensator.RegulationMode regulationMode;

    @Schema(description = "Voltage set point")
    private Double voltageSetpoint;

    @Schema(description = "Reactive power set point")
    private Double reactivePowerSetpoint;

    @Schema(description = "Voltage Regulation type")
    private VoltageRegulationType voltageRegulationType;

    @Schema(description = "Regulating terminal equipment id")
    private String regulatingTerminalId;

    @Schema(description = "Regulating terminal equipment type")
    private String regulatingTerminalType;

    @Schema(description = "Regulating terminal voltage level id")
    private String regulatingTerminalVlId;

    @Schema(description = "add standby automate")
    private boolean standByAutomateOn;

    @Schema(description = "Minimum reactive power ")
    private boolean standby;

    @Schema(description = "Fix part of the susceptance")
    private Double b0;

    @Schema(description = "Fix part of the reactive power")
    private Double q0;

    @Schema(description = "Low voltage setpoint ")
    private Double lowVoltageSetpoint;

    @Schema(description = "High voltage setpoint")
    private Double highVoltageSetpoint;

    @Schema(description = "Low voltage threshold")
    private Double lowVoltageThreshold;

    @Schema(description = "High voltage threshold")
    private Double highVoltageThreshold;

    @Override
    public StaticCompensatorCreationEntity toEntity() {
        return new StaticCompensatorCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new StaticVarCompensatorCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate(ModificationType.STATIC_VAR_COMPENSATOR_CREATION.name(), "Static Compensator ${staticCompensatorId}").withUntypedValue("staticCompensatorId", this.getEquipmentId()).add();
    }
}
