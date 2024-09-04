package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.PhaseTapChanger;
import com.powsybl.iidm.network.RatioTapChanger;
import com.powsybl.iidm.network.TwoWindingsTransformer;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;
import static org.gridsuite.modification.server.modifications.TwoWindingsTransformerModification.*;

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
        final AttributeModification<Double> attrModif = new AttributeModification<>(newValue, OperationType.SET);

        switch (field) {
            case R -> modifyBranchFields(transformer, attrModif, null, null);
            case X -> modifyBranchFields(transformer, null, attrModif, null);
            case G -> modifyG(transformer, attrModif, null);
            case B -> modifyB(transformer, attrModif, null);
            case RATED_U1 -> modifyRatedU1(transformer, attrModif, null);
            case RATED_U2 -> modifyRatedU2(transformer, attrModif, null);
            case RATED_S -> modifyRatedS(transformer, attrModif, null);
            case TARGET_V -> modifyTargets(ratioTapChanger, null, true, attrModif, null, null);
            case RATIO_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                    new AttributeModification<>(newValue.intValue(), OperationType.SET), null, null, null);
            case RATIO_TAP_POSITION -> processTapChangerPositionsAndSteps(ratioTapChanger, null, true,
                    null, new AttributeModification<>(newValue.intValue(), OperationType.SET), null, null);
            case RATIO_TARGET_DEADBAND -> modifyTargets(ratioTapChanger, null, true, null, attrModif, null);
            case REGULATION_VALUE -> processPhaseTapRegulation(
                    phaseTapChanger, null, null, true, attrModif, null, null
            );
            case PHASE_LOW_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                    new AttributeModification<>(newValue.intValue(), OperationType.SET), null, null, null);
            case PHASE_TAP_POSITION -> processTapChangerPositionsAndSteps(phaseTapChanger, null, true,
                    null, new AttributeModification<>(newValue.intValue(), OperationType.SET), null, null);
            case PHASE_TARGET_DEADBAND -> processPhaseTapRegulation(
                    phaseTapChanger, null, null, true, null, attrModif, null
            );
        }
    }
}
