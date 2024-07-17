/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.SwitchKind;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.VoltageLevelCreation;

import java.util.List;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Voltage level creation")
@JsonTypeName("VOLTAGE_LEVEL_CREATION")
@ModificationErrorTypeName("CREATE_VOLTAGE_LEVEL_ERROR")
public class VoltageLevelCreationInfos extends EquipmentCreationInfos {

    @Schema(description = "substation id")
    private String substationId;

    @Schema(description = "nominal voltage in kV")
    private double nominalV;

    @Schema(description = "low voltage limit in kV")
    private Double lowVoltageLimit;

    @Schema(description = "high voltage limit  in kV")
    private Double highVoltageLimit;

    @Schema(description = "low short-circuit current limit in A")
    private Double ipMin;

    @Schema(description = "high short-circuit current limit in A")
    private Double ipMax;

    @Schema(description = "busbar Count")
    private int busbarCount;

    @Schema(description = "section Count")
    private int sectionCount;

    @Schema(description = "switchKinds")
    private List<SwitchKind> switchKinds;

    @Schema(description = "coupling devices infos")
    private List<CouplingDeviceInfos> couplingDevices;

    @Override
    public VoltageLevelCreationEntity toEntity() {
        return new VoltageLevelCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "VoltageLevel creation ${voltageLevelId}")
                .withUntypedValue("voltageLevelId", getEquipmentId())
                .add();
    }
}
