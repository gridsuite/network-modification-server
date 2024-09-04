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
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;
import org.gridsuite.modification.server.dto.ShuntCompensatorType;

import static org.gridsuite.modification.server.modifications.ShuntCompensatorModification.*;

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
        var shuntCompensatorType = model.getBPerSection() > 0 ? ShuntCompensatorType.CAPACITOR : ShuntCompensatorType.REACTOR;
        switch (field) {
            case MAXIMUM_SECTION_COUNT -> modifyMaximumSectionCount(new AttributeModification<>(newValue.intValue(), OperationType.SET),
                    null, null, null, shuntCompensator, model);
            case SECTION_COUNT -> modifySectionCount(new AttributeModification<>(newValue.intValue(), OperationType.SET), null, shuntCompensator);
            case MAXIMUM_SUSCEPTANCE -> modifyMaxSusceptance(new AttributeModification<>(newValue, OperationType.SET),
                    shuntCompensator.getMaximumSectionCount(), null, model);
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> modifyMaximumQAtNominalVoltage(new AttributeModification<>(newValue, OperationType.SET),
                    voltageLevel, shuntCompensator.getMaximumSectionCount(), null, model, shuntCompensatorType);
        }
    }
}
