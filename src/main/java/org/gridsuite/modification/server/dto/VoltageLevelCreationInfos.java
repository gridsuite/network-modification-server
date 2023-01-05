/*
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import java.util.List;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.VoltageLevelCreation;

/**
 * @author Laurent GARNIER <laurent.garnier at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Voltage level creation")
public class VoltageLevelCreationInfos extends EquipmentCreationInfos {

    @Schema(description = "nominal voltage in kV")
    private double nominalVoltage;

    @Schema(description = "substation id")
    private String substationId;

    @Schema(description = "bus bar sections")
    private List<BusbarSectionCreationInfos> busbarSections;

    @Schema(description = "connections between bus bar sections")
    private List<BusbarConnectionCreationInfos> busbarConnections;

    @Override
    public VoltageLevelCreationEntity toEntity() {
        return new VoltageLevelCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new VoltageLevelCreation(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.VOLTAGE_LEVEL_CREATION.name(), "VoltageLevel creation ${voltageLevelId}", "voltageLevelId", this.getEquipmentId());
    }
}
