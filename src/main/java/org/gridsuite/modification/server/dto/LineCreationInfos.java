/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

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
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LineCreation;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line creation")
public class LineCreationInfos extends BranchCreationInfos {

    @Schema(description = "Shunt conductance Side 1")
    private Double shuntConductance1;

    @Schema(description = "Shunt susceptance Side 1")
    private Double shuntSusceptance1;

    @Schema(description = "Shunt conductance Side 2")
    private Double shuntConductance2;

    @Schema(description = "Shunt susceptance Side 2")
    private Double shuntSusceptance2;

    @Override
    public LineCreationEntity toEntity() {
        return new LineCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new LineCreation(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.CREATE_LINE_ERROR;
    }

    @Override
    public ModificationType getType() {
        return ModificationType.LINE_CREATION;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.LINE_CREATION.name(), "Creation of line " + getEquipmentId());
    }
}
