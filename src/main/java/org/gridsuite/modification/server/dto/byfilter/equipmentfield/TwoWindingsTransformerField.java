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

        switch (field) {
            case R -> transformer.setR(Double.parseDouble(newValue));
            case X -> transformer.setX(Double.parseDouble(newValue));
            case G -> transformer.setG(Double.parseDouble(newValue));
            case B -> transformer.setB(Double.parseDouble(newValue));
            case RATED_U1 -> transformer.setRatedU1(Double.parseDouble(newValue));
            case RATED_U2 -> transformer.setRatedU2(Double.parseDouble(newValue));
            case RATED_S -> transformer.setRatedS(Double.parseDouble(newValue));
            case TARGET_V -> ratioTapChanger.setTargetV(Double.parseDouble(newValue));
            case RATIO_LOW_TAP_POSITION -> ratioTapChanger.setLowTapPosition((int) Double.parseDouble(newValue));
            case RATIO_TAP_POSITION -> ratioTapChanger.setTapPosition((int) Double.parseDouble(newValue));
            case RATIO_TARGET_DEADBAND -> ratioTapChanger.setTargetDeadband(Double.parseDouble(newValue));
            case REGULATION_VALUE -> phaseTapChanger.setRegulationValue(Double.parseDouble(newValue));
            case PHASE_LOW_TAP_POSITION -> phaseTapChanger.setLowTapPosition((int) Double.parseDouble(newValue));
            case PHASE_TAP_POSITION -> phaseTapChanger.setTapPosition((int) Double.parseDouble(newValue));
            case PHASE_TARGET_DEADBAND -> phaseTapChanger.setTargetDeadband(Double.parseDouble(newValue));
        }
    }
}
