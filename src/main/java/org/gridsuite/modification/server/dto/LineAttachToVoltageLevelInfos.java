/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.powsybl.commons.reporter.Reporter;
import com.powsybl.commons.reporter.ReporterModel;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LineAttachToVoltageLevel;

/**
 * @author Nicolas NOIR <nicolas.noir at rte-france.com>
 */
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line attach to voltage level")
public class LineAttachToVoltageLevelInfos extends ModificationInfos {

    @Schema(description = "line to attach to ID")
    private String lineToAttachToId;

    @Schema(description = "percentage of line length from side 1")
    private double percent;

    @Schema(description = "attachment point id")
    private String attachmentPointId;

    @Schema(description = "attachment point name")
    private String attachmentPointName;

    @Schema(description = "possible new voltage level to create before inserting it, may be null")
    private VoltageLevelCreationInfos mayNewVoltageLevelInfos;

    @Schema(description = "if no new voltage level, ID for the existing voltage level")
    private String existingVoltageLevelId;

    @Schema(description = "bus bar section or bus id")
    private String bbsOrBusId;

    @Schema(description = "attachment line")
    private LineCreationInfos attachmentLine;

    @Schema(description = "new line 1 ID")
    private String newLine1Id;

    @Schema(description = "new line 1 name")
    private String newLine1Name;

    @Schema(description = "new line 1 ID")
    private String newLine2Id;

    @Schema(description = "new line 2 name")
    private String newLine2Name;

    @Override
    public LineAttachToVoltageLevelEntity toEntity() {
        return new LineAttachToVoltageLevelEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new LineAttachToVoltageLevel(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.LINE_ATTACH_ERROR;
    }

    @Override
    public ModificationType getType() {
        return ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL.name(), "Line attach to voltage level");
    }
}
