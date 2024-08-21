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
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.dto.byfilter.simple.SimpleModificationByFilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.BySimpleModificationEntity;
import org.gridsuite.modification.server.modifications.byfilter.BySimpleModification;

import java.util.List;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonTypeName("BY_SIMPLE_MODIFICATION")
@ModificationErrorTypeName("BY_SIMPLE_MODIFICATION_ERROR")
@ToString(callSuper = true)
@Schema(description = "Modification by simple assignment")
public class BySimpleModificationInfos extends ModificationInfos {
    @Schema(description = "Identifiable type")
    private IdentifiableType identifiableType;

    @Schema(description = "list of modifications")
    private List<? extends SimpleModificationByFilterInfos<?>> simpleModificationInfosList;

    @Override
    public BySimpleModificationEntity toEntity() {
        return new BySimpleModificationEntity(this);
    }

    @Override
    public BySimpleModification toModification() {
        return new BySimpleModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode().withMessageTemplate(ModificationType.BY_SIMPLE_MODIFICATION.name(), "By filter modification").add();
    }
}
