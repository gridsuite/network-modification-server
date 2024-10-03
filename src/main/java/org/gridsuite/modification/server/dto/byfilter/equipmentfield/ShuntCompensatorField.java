/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.ShuntCompensatorModelType;
import com.powsybl.iidm.network.VoltageLevel;
import jakarta.validation.constraints.NotNull;
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

    public static String getReferenceValue(ShuntCompensator shuntCompensator, String shuntCompensatorField) {
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        return switch (field) {
            case MAXIMUM_SECTION_COUNT -> String.valueOf(shuntCompensator.getMaximumSectionCount());
            case SECTION_COUNT -> String.valueOf(shuntCompensator.getSectionCount());
            case MAXIMUM_SUSCEPTANCE -> String.valueOf(shuntCompensator.getB() * shuntCompensator.getMaximumSectionCount());
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> String.valueOf(Math.abs(Math.pow(voltageLevel.getNominalV(), 2) * shuntCompensator.getB()) * shuntCompensator.getMaximumSectionCount());
        };
    }

    public static void setNewValue(ShuntCompensator shuntCompensator, String shuntCompensatorField, @NotNull String newValue) {
        if (shuntCompensator.getModelType() != ShuntCompensatorModelType.LINEAR) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                    String.format("Shunt compensator with %s model is not supported", shuntCompensator.getModelType()));
        }
        ShuntCompensatorLinearModel model = shuntCompensator.getModel(ShuntCompensatorLinearModel.class);
        ShuntCompensatorField field = ShuntCompensatorField.valueOf(shuntCompensatorField);
        VoltageLevel voltageLevel = shuntCompensator.getTerminal().getVoltageLevel();
        var shuntCompensatorType = ShuntCompensatorType.REACTOR;
        if (model != null && model.getBPerSection() > 0) {
            shuntCompensatorType = ShuntCompensatorType.CAPACITOR;
        }
        switch (field) {
            case MAXIMUM_SECTION_COUNT -> modifyMaximumSectionCount(new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET),
                    null, null, null, shuntCompensator, model);
            case SECTION_COUNT -> modifySectionCount(new AttributeModification<>((int) Double.parseDouble(newValue), OperationType.SET), null, shuntCompensator);
            case MAXIMUM_SUSCEPTANCE -> modifyMaxSusceptance(new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET),
                    shuntCompensator.getMaximumSectionCount(), null, model);
            case MAXIMUM_Q_AT_NOMINAL_VOLTAGE -> modifyMaximumQAtNominalVoltage(new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET),
                    voltageLevel, shuntCompensator.getMaximumSectionCount(), null, model, shuntCompensatorType);
        }
    }
}
