/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.RatioTapChanger;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import jakarta.validation.constraints.NotNull;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import static org.gridsuite.modification.server.modifications.TwoWindingsTransformerModification.*;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum TwoWindingsTransformerField {
    R,
    X,
    G,
    B,
    RATED_U1,
    RATED_U2,
    RATED_S,
    TARGET_V,
    RATIO_LOW_TAP_POSITION,
    RATIO_TAP_POSITION,
    RATIO_TARGET_DEADBAND,
    REGULATION_VALUE,
    PHASE_LOW_TAP_POSITION,
    PHASE_TAP_POSITION,
    PHASE_TARGET_DEADBAND;

    public static String getReferenceValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();
        return switch (field) {
            case R -> String.valueOf(transformer.getR());
            case X -> String.valueOf(transformer.getX());
            case G -> String.valueOf(transformer.getG());
            case B -> String.valueOf(transformer.getB());
            case RATED_U1 -> String.valueOf(transformer.getRatedU1());
            case RATED_U2 -> String.valueOf(transformer.getRatedU2());
            case RATED_S -> String.valueOf(transformer.getRatedS());
            case TARGET_V -> ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTargetV()) : null;
            case RATIO_LOW_TAP_POSITION -> ratioTapChanger != null ? String.valueOf(ratioTapChanger.getLowTapPosition()) : null;
            case RATIO_TAP_POSITION -> ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTapPosition()) : null;
            case RATIO_TARGET_DEADBAND -> ratioTapChanger != null ? String.valueOf(ratioTapChanger.getTargetDeadband()) : null;
            case REGULATION_VALUE -> phaseTapChanger != null ? String.valueOf(phaseTapChanger.getRegulationValue()) : null;
            case PHASE_LOW_TAP_POSITION -> phaseTapChanger != null ? String.valueOf(phaseTapChanger.getLowTapPosition()) : null;
            case PHASE_TAP_POSITION -> phaseTapChanger != null ? String.valueOf(phaseTapChanger.getTapPosition()) : null;
            case PHASE_TARGET_DEADBAND -> phaseTapChanger != null ? String.valueOf(phaseTapChanger.getTargetDeadband()) : null;
        };
    }

    public static void setNewValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField, @NotNull String newValue) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();
        final AttributeModification<Double> attributeModification = new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET);

        switch (field) {
            case R -> modifyR(transformer, attributeModification, null);
            case X -> modifyX(transformer, attributeModification, null);
            case G -> modifyG(transformer, attributeModification, null);
            case B -> modifyB(transformer, attributeModification, null);
            case RATED_U1 -> modifyRatedU1(transformer, attributeModification, null);
            case RATED_U2 -> modifyRatedU2(transformer, attributeModification, null);
            case RATED_S -> modifyRatedS(transformer, attributeModification, null);
            case TARGET_V -> modifyTargets(ratioTapChanger, null, true, attributeModification, null, null);
            case RATIO_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                    new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null, null);
            case RATIO_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                    null, new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null);
            case RATIO_TARGET_DEADBAND -> modifyTargets(ratioTapChanger, null, true, null, attributeModification, null);
            case REGULATION_VALUE -> processPhaseTapRegulation(
                    phaseTapChanger, null, true, null, attributeModification, null, null);
            case PHASE_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                    new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null, null);
            case PHASE_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                    null, new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, null);
            case PHASE_TARGET_DEADBAND -> processPhaseTapRegulation(
                    phaseTapChanger, null, true, null, null, attributeModification, null
            );
        }
    }
}
