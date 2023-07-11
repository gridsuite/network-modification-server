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
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.entities.equipment.modification.LineModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.LineModification;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Line modification")
@JsonTypeName("LINE_MODIFICATION")
public class LineModificationInfos extends BranchModificationInfos {

    @Schema(description = "Shunt conductance Side 1")
    private AttributeModification<Double> shuntConductance1;

    @Schema(description = "Shunt susceptance Side 1")
    private AttributeModification<Double> shuntSusceptance1;

    @Schema(description = "Shunt conductance Side 2")
    private AttributeModification<Double> shuntConductance2;

    @Schema(description = "Shunt susceptance Side 2")
    private AttributeModification<Double> shuntSusceptance2;

    @Override
    public LineModificationEntity toEntity() {
        return new LineModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new LineModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFY_LINE_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.LINE_MODIFICATION.name(), "Line modification ${lineId}", "lineId", this.getEquipmentId());
    }
}
