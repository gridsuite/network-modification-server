/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
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
public class TwoWindingsTransformerModificationInfos extends BranchModificationInfos {

    @Schema(description = "Magnetizing conductance")
    private AttributeModification<Double> magnetizingConductance;

    @Schema(description = "Magnetizing susceptance")
    private AttributeModification<Double> magnetizingSusceptance;

    @Schema(description = "Side 1 rated voltage")
    private AttributeModification<Double> ratedVoltage1;

    @Schema(description = "Side 2 rated voltage")
    private AttributeModification<Double> ratedVoltage2;

    @Schema(description = "Rated conductance in Siemens")
    private AttributeModification<Double> ratedS;

    @Schema(description = "Ratio tap changer")
    private RatioTapChangerModificationInfos ratioTapChanger;

    @Override
    public TwoWindingsTransformerModificationEntity toEntity() {
        return new TwoWindingsTransformerModificationEntity(this);
    }

    @Override
    public AbstractModification toModification() {
        return new TwoWindingsTransformerModification(this);
    }

    @Override
    public NetworkModificationException.Type getErrorType() {
        return NetworkModificationException.Type.MODIFY_TWO_WINDINGS_TRANSFORMER_ERROR;
    }

    @Override
    public Reporter createSubReporter(ReporterModel reporter) {
        return reporter.createSubReporter(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION.name(), "Two windings transformer modification ${twoWindingsTransformerId}", "twoWindingsTransformerId", getEquipmentId());
    }

}
