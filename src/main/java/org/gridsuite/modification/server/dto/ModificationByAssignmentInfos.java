/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ModificationByAssignmentEntity;
import org.gridsuite.modification.server.modifications.byfilter.ModificationByAssignment;

import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@JsonTypeName("MODIFICATION_BY_ASSIGNMENT")
@ModificationErrorTypeName("MODIFICATION_BY_ASSIGNMENT_ERROR")
@ToString(callSuper = true)
@Schema(description = "Modification by assignment")
public class ModificationByAssignmentInfos extends ModificationInfos {
    @Schema(description = "Equipment type")
    private IdentifiableType equipmentType;

    @Schema(description = "list of modifications")
    private List<? extends AssignmentInfos<?>> assignmentInfosList;

    @Override
    public ModificationByAssignmentEntity toEntity() {
        return new ModificationByAssignmentEntity(this);
    }

    @Override
    public ModificationByAssignment toModification() {
        return new ModificationByAssignment(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate(ModificationType.MODIFICATION_BY_ASSIGNMENT.name(), "Modification by filter").add();
    }
}
