/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.modification.TwoWindingsTransformerModificationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.TwoWindingsTransformerModification;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Two windings transformer modification")
@JsonTypeName("TWO_WINDINGS_TRANSFORMER_MODIFICATION")
@ModificationErrorTypeName("MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerModificationInfos extends BranchModificationInfos {

    @Schema(description = "Magnetizing conductance")
    private AttributeModification<Double> g;

    @Schema(description = "Magnetizing susceptance")
    private AttributeModification<Double> b;

    @Schema(description = "Side 1 rated voltage")
    private AttributeModification<Double> ratedU1;

    @Schema(description = "Side 2 rated voltage")
    private AttributeModification<Double> ratedU2;

    @Schema(description = "Rated conductance in Siemens")
    private AttributeModification<Double> ratedS;

    @Schema(description = "Ratio tap changer")
    @Builder.Default
    private RatioTapChangerModificationInfos ratioTapChanger = new RatioTapChangerModificationInfos();

    @Schema(description = "Phase tap changer")
    @Builder.Default
    private PhaseTapChangerModificationInfos phaseTapChanger = new PhaseTapChangerModificationInfos();

    @Override
    public TwoWindingsTransformerModificationEntity toEntity() {
        return new TwoWindingsTransformerModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerModification(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(getType().name(), "Two windings transformer modification ${equipmentId}")
                .withUntypedValue("equipmentId", getEquipmentId())
                .add();
    }
}
