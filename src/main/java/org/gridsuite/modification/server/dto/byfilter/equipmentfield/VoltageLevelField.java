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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_ERROR;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum VoltageLevelField {
    NOMINAL_VOLTAGE,
    LOW_VOLTAGE_LIMIT,
    HIGH_VOLTAGE_LIMIT,
    LOW_SHORT_CIRCUIT_CURRENT_LIMIT,
    HIGH_SHORT_CIRCUIT_CURRENT_LIMIT;

    public static final String UNSUPPORTED_VOLTAGE_LEVEL_DATA_TYPE_ERROR_MESSAGE = "Unsupported voltage level data type: ";

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
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit = voltageLevel.getExtension(IdentifiableShortCircuit.class);
        VoltageLevelField field = VoltageLevelField.valueOf(voltageLevelField);
        switch (field) {
            case NOMINAL_VOLTAGE -> voltageLevel.setNominalV(newValue);
            case LOW_VOLTAGE_LIMIT -> voltageLevel.setLowVoltageLimit(newValue);
            case HIGH_VOLTAGE_LIMIT -> voltageLevel.setHighVoltageLimit(newValue);
            case LOW_SHORT_CIRCUIT_CURRENT_LIMIT -> {
                IdentifiableShortCircuitAdder<VoltageLevel> adder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class).withIpMin(newValue);
                if (identifiableShortCircuit != null) {
                    adder.withIpMax(identifiableShortCircuit.getIpMax());
                }
                adder.add();
            }
            case HIGH_SHORT_CIRCUIT_CURRENT_LIMIT -> {
                IdentifiableShortCircuitAdder<VoltageLevel> adder = voltageLevel.newExtension(IdentifiableShortCircuitAdder.class).withIpMax(newValue);
                if (identifiableShortCircuit != null) {
                    adder.withIpMin(identifiableShortCircuit.getIpMin());
                }
                adder.add();
            }
        }
    }

    public static void setNewValue(VoltageLevel voltageLevel, AbstractSimpleModificationByFilterInfos<?> simpleModificationByFilterInfos) {
        switch (simpleModificationByFilterInfos.getDataType()) {
            case DOUBLE, INTEGER -> setNewValue(voltageLevel, simpleModificationByFilterInfos.getEditedField(), (Double) simpleModificationByFilterInfos.getValue());
            default -> throw new NetworkModificationException(MODIFICATION_ERROR, UNSUPPORTED_VOLTAGE_LEVEL_DATA_TYPE_ERROR_MESSAGE + simpleModificationByFilterInfos.getDataType());
        }
    }
}