/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula.equipmentfield;

import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import static org.gridsuite.modification.server.modifications.VoltageLevelModification.modifyVoltageLevelShortCircuit;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum VoltageLevelField {
    NOMINAL_VOLTAGE,
    LOW_VOLTAGE_LIMIT,
    HIGH_VOLTAGE_LIMIT,
    LOW_SHORT_CIRCUIT_CURRENT_LIMIT,
    HIGH_SHORT_CIRCUIT_CURRENT_LIMIT;

    public static Double getReferenceValue(VoltageLevel voltageLevel, String voltageLevelField) {
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        return switch (field) {
            case NOMINAL_VOLTAGE -> voltageLevel.getNominalV();
            case LOW_VOLTAGE_LIMIT -> voltageLevel.getLowVoltageLimit();
            case HIGH_VOLTAGE_LIMIT -> voltageLevel.getHighVoltageLimit();
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit == null ? null : identifiableShortCircuit.getIpMin();
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> identifiableShortCircuit == null ? null : identifiableShortCircuit.getIpMax();
        };
    }

    public static void setNewValue(VoltageLevel voltageLevel, String voltageLevelField, Double newValue) {
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        switch (field) {
            case NOMINAL_VOLTAGE -> voltageLevel.setNominalV(newValue);
            case LOW_VOLTAGE_LIMIT -> voltageLevel.setLowVoltageLimit(newValue);
            case HIGH_VOLTAGE_LIMIT -> voltageLevel.setHighVoltageLimit(newValue);
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    new AttributeModification<>(newValue, OperationType.SET), null, null, voltageLevel);
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> modifyVoltageLevelShortCircuit(
                    null, new AttributeModification<>(newValue, OperationType.SET), null, voltageLevel);
        }
    }
}
