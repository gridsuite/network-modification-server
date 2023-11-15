package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.VoltageLevel;

public enum ShuntCompensatorField {
    MAXIMUM_SECTION_COUNT,
    SECTION_COUNT,
    MAXIMUM_SUSCEPTANCE,
    MAXIMUM_Q_AT_NOMINAL_VOLTAGE;

    public static Double getReferenceValue(ShuntCompensator shuntCompensator, String shuntCompensatorField, VoltageLevel voltageLevel) {
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        return switch (field) {
            case MAXIMUM_SECTION_COUNT -> (double) shuntCompensator.getMaximumSectionCount();
            case SECTION_COUNT -> (double) shuntCompensator.getSectionCount();
            case MAXIMUM_SUSCEPTANCE -> shuntCompensator.getB() * shuntCompensator.getMaximumSectionCount();
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * shuntCompensator.getB()) * shuntCompensator.getMaximumSectionCount();
        };
    }
}