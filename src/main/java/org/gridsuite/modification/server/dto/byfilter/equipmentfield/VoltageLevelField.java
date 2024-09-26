/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import static org.gridsuite.modification.server.modifications.VoltageLevelModification.*;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import jakarta.validation.constraints.NotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum VoltageLevelField {
    NOMINAL_VOLTAGE,
    LOW_VOLTAGE_LIMIT,
    HIGH_VOLTAGE_LIMIT,
    LOW_SHORT_CIRCUIT_CURRENT_LIMIT,
    HIGH_SHORT_CIRCUIT_CURRENT_LIMIT;

    public static String getReferenceValue(VoltageLevel voltageLevel, String voltageLevelField) {
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        return switch (field) {
            case NOMINAL_VOLTAGE -> String.valueOf(voltageLevel.getNominalV());
            case LOW_VOLTAGE_LIMIT -> String.valueOf(voltageLevel.getLowVoltageLimit());
            case HIGH_VOLTAGE_LIMIT -> String.valueOf(voltageLevel.getHighVoltageLimit());
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit != null ? String.valueOf(identifiableShortCircuit.getIpMin()) : null;
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit != null ? String.valueOf(identifiableShortCircuit.getIpMax()) : null;
        };
    }

    public static void setNewValue(VoltageLevel voltageLevel, String voltageLevelField, @NotNull String newValue) {
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        switch (field) {
            case NOMINAL_VOLTAGE -> modifyNominalV(voltageLevel, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null);
            case LOW_VOLTAGE_LIMIT -> modifLowVoltageLimit(voltageLevel, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null);
            case HIGH_VOLTAGE_LIMIT -> modifyHighVoltageLimit(voltageLevel, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null);
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null, null, voltageLevel);
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    null, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null, voltageLevel);
        }
    }
}
