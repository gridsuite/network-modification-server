/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.server.NetworkModificationException;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum ShuntCompensatorField {
    MAXIMUM_SECTION_COUNT,
    SECTION_COUNT,
    MAXIMUM_SUSCEPTANCE,
    MAXIMUM_Q_AT_NOMINAL_VOLTAGE;

    public static Double getReferenceValue(ShuntCompensator shuntCompensator, String shuntCompensatorField) {
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        return switch (field) {
            case MAXIMUM_SECTION_COUNT -> (double) shuntCompensator.getMaximumSectionCount();
            case SECTION_COUNT -> (double) shuntCompensator.getSectionCount();
            case MAXIMUM_SUSCEPTANCE -> shuntCompensator.getB() * shuntCompensator.getMaximumSectionCount();
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * shuntCompensator.getB()) * shuntCompensator.getMaximumSectionCount();
        };
    }

    public static void setNewValue(ShuntCompensator shuntCompensator, String shuntCompensatorField, Double newValue) {
        if (shuntCompensator.getModelType() != ShuntCompensatorModelType.LINEAR) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                    String.format("Shunt compensator with %s model is not supported", shuntCompensator.getModelType()));
        }
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        switch (field) {
            case MAXIMUM_SECTION_COUNT -> {
                int maximumSectionCount = newValue.intValue();
                model.setBPerSection(model.getBPerSection() * shuntCompensator.getMaximumSectionCount() / maximumSectionCount);
                model.setMaximumSectionCount(maximumSectionCount);
            }
            case SECTION_COUNT -> shuntCompensator.setSectionCount(newValue.intValue());
            case MAXIMUM_SUSCEPTANCE -> model.setBPerSection(newValue / shuntCompensator.getMaximumSectionCount());
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> {
                double newQatNominalV = newValue / shuntCompensator.getMaximumSectionCount();
                double newSusceptancePerSection = newQatNominalV / Math.pow(voltageLevel.getNominalV(), 2);
                model.setBPerSection(newSusceptancePerSection);
            }
        }
    }
}
