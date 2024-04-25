/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.commons.report.ReportNode;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.entities.equipment.creation.TwoWindingsTransformerCreationEntity;
import org.gridsuite.modification.server.modifications.AbstractModification;
import org.gridsuite.modification.server.modifications.TwoWindingsTransformerCreation;

/**
 * @author Abdelsalem Hedhili <abdelsalem.hedhili at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Two windings transformer creation")
@JsonTypeName("TWO_WINDINGS_TRANSFORMER_CREATION")
@ModificationErrorTypeName("CREATE_TWO_WINDINGS_TRANSFORMER_ERROR")
public class TwoWindingsTransformerCreationInfos extends BranchCreationInfos {

    @Schema(description = "Magnetizing conductance")
    private double g;

    @Schema(description = "Magnetizing susceptance")
    private double b;

    @Schema(description = "side 1 rated voltage")
    private double ratedU1;

    @Schema(description = "side 2 rated voltage")
    private double ratedU2;

    @Schema(description = "Rated conductance in Siemens")
    private Double ratedS;

    @Schema(description = "Ratio tap changer")
    private RatioTapChangerCreationInfos ratioTapChanger;

    @Schema(description = "Phase tap changer")
    private PhaseTapChangerCreationInfos phaseTapChanger;

    @Override
    public TwoWindingsTransformerCreationEntity toEntity() {
        return new TwoWindingsTransformerCreationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerCreation(this);
    }

    @Override
    public ReportNode createSubReportNode(ReportNode reportNode) {
        return reportNode.newReportNode()
                .withMessageTemplate(ModificationType.TABULAR_CREATION.name(), "Two windings transformer creation ${twoWindingsTransformerId}")
                .withUntypedValue("twoWindingsTransformerId", getEquipmentId())
                .add();
    }

}
