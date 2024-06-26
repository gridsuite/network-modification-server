package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.RatioTapChanger;
import com.powsybl.iidm.network.TwoWindingsTransformer;

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

    public static Double getReferenceValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();
        return switch (field) {
            case R -> transformer.getR();
            case X -> transformer.getX();
            case G -> transformer.getG();
            case B -> transformer.getB();
            case RATED_U1 -> transformer.getRatedU1();
            case RATED_U2 -> transformer.getRatedU2();
            case RATED_S -> transformer.getRatedS();
            case TARGET_V -> ratioTapChanger != null ? ratioTapChanger.getTargetV() : null;
            case RATIO_LOW_TAP_POSITION -> ratioTapChanger != null ? (double) ratioTapChanger.getLowTapPosition() : null;
            case RATIO_TAP_POSITION -> ratioTapChanger != null ? (double) ratioTapChanger.getTapPosition() : null;
            case RATIO_TARGET_DEADBAND -> ratioTapChanger != null ? ratioTapChanger.getTargetDeadband() : null;
            case REGULATION_VALUE -> phaseTapChanger != null ? phaseTapChanger.getRegulationValue() : null;
            case PHASE_LOW_TAP_POSITION -> phaseTapChanger != null ? (double) phaseTapChanger.getLowTapPosition() : null;
            case PHASE_TAP_POSITION -> phaseTapChanger != null ? (double) phaseTapChanger.getTapPosition() : null;
            case PHASE_TARGET_DEADBAND -> phaseTapChanger != null ? phaseTapChanger.getTargetDeadband() : null;
        };
    }

    public static void setNewValue(TwoWindingsTransformer transformer, String twoWindingsTransformerField, Double newValue) {
        TwoWindingsTransformerField field = TwoWindingsTransformerField.valueOf(twoWindingsTransformerField);
        final PhaseTapChanger phaseTapChanger = transformer.getPhaseTapChanger();
        final RatioTapChanger ratioTapChanger = transformer.getRatioTapChanger();

        switch (field) {
            case R -> transformer.setR(newValue);
            case X -> transformer.setX(newValue);
            case G -> transformer.setG(newValue);
            case B -> transformer.setB(newValue);
            case RATED_U1 -> transformer.setRatedU1(newValue);
            case RATED_U2 -> transformer.setRatedU2(newValue);
            case RATED_S -> transformer.setRatedS(newValue);
            case TARGET_V -> ratioTapChanger.setTargetV(newValue);
            case RATIO_LOW_TAP_POSITION -> ratioTapChanger.setLowTapPosition(newValue.intValue());
            case RATIO_TAP_POSITION -> ratioTapChanger.setTapPosition(newValue.intValue());
            case RATIO_TARGET_DEADBAND -> ratioTapChanger.setTargetDeadband(newValue);
            case REGULATION_VALUE -> phaseTapChanger.setRegulationValue(newValue);
            case PHASE_LOW_TAP_POSITION -> phaseTapChanger.setLowTapPosition(newValue.intValue());
            case PHASE_TAP_POSITION -> phaseTapChanger.setTapPosition(newValue.intValue());
            case PHASE_TARGET_DEADBAND -> phaseTapChanger.setTargetDeadband(newValue);
        }
    }
}
