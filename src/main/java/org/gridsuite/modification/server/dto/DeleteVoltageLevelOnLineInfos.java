/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.DeleteVoltageLevelOnLine;


/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Delete voltage level on line infos")
@JsonTypeName("DELETE_VOLTAGE_LEVEL_ON_LINE")
@ModificationErrorTypeName("DELETE_VOLTAGE_LEVEL_ON_LINE_ERROR")
public class DeleteVoltageLevelOnLineInfos extends ModificationInfos {

    @Schema(description = "line 1 id")
    private String lineToAttachTo1Id;

    @Schema(description = "line 2 id")
    private String lineToAttachTo2Id;

    @Schema(description = "replacing line 1 ID")
    private String replacingLine1Id;

    @Schema(description = "replacing line 1 name")
    private String replacingLine1Name;

    @Override
    public DeleteVoltageLevelOnLineEntity toEntity() {
        return new DeleteVoltageLevelOnLineEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new DeleteVoltageLevelOnLine(this);
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(getType().name(), "Delete voltage level on line");
    }
}
