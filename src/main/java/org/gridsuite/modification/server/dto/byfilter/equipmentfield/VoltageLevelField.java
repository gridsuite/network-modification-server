/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;

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

    public static void setNewValue(VoltageLevel voltageLevel, String voltageLevelField, String newValue) {
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        switch (field) {
            case NOMINAL_VOLTAGE -> voltageLevel.setNominalV(Double.parseDouble(newValue));
            case LOW_VOLTAGE_LIMIT -> voltageLevel.setLowVoltageLimit(Double.parseDouble(newValue));
            case HIGH_VOLTAGE_LIMIT -> voltageLevel.setHighVoltageLimit(Double.parseDouble(newValue));
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> {
                IdentifiableShortCircuitAdder<VoltageLevel> adder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class).withIpMin(Double.parseDouble(newValue));
                if (identifiableShortCircuit != null) {
                    adder.withIpMax(identifiableShortCircuit.getIpMax());
                }
                adder.add();
            }
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> {
                IdentifiableShortCircuitAdder<VoltageLevel> adder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class).withIpMax(Double.parseDouble(newValue));
                if (identifiableShortCircuit != null) {
                    adder.withIpMin(identifiableShortCircuit.getIpMin());
                }
                adder.add();
            }
        }
    }
}
