/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.*;
import org.gridsuite.modification.server.NetworkModificationException;

import javax.annotation.Nullable;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public final class FieldUtils {

    private FieldUtils() {

    }

    @Nullable
    public static String getFieldValue(Identifiable<?> equipment, String equipmentField) {
        return switch (equipment.getType()) {
            case GENERATOR -> GeneratorField.getReferenceValue((Generator) equipment, equipmentField);
            case BATTERY -> BatteryField.getReferenceValue((Battery) equipment, equipmentField);
            case SHUNT_COMPENSATOR ->
                    ShuntCompensatorField.getReferenceValue((ShuntCompensator) equipment, equipmentField);
            case VOLTAGE_LEVEL -> VoltageLevelField.getReferenceValue((VoltageLevel) equipment, equipmentField);
            case LOAD -> LoadField.getReferenceValue((Load) equipment, equipmentField);
            case TWO_WINDINGS_TRANSFORMER ->
                    TwoWindingsTransformerField.getReferenceValue((TwoWindingsTransformer) equipment, equipmentField);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                            "Unsupported getting value for equipment type : " + equipment.getType().name());
        };
    }

    public static void setFieldValue(Identifiable<?> equipment, String equipmentField, String newValue) {
        switch (equipment.getType()) {
            case GENERATOR -> GeneratorField.setNewValue((Generator) equipment, equipmentField, newValue);
            case BATTERY -> BatteryField.setNewValue((Battery) equipment, equipmentField, newValue);
            case SHUNT_COMPENSATOR -> ShuntCompensatorField.setNewValue((ShuntCompensator) equipment, equipmentField, newValue);
            case VOLTAGE_LEVEL -> VoltageLevelField.setNewValue((VoltageLevel) equipment, equipmentField, newValue);
            case LOAD -> LoadField.setNewValue((Load) equipment, equipmentField, newValue);
            case TWO_WINDINGS_TRANSFORMER -> TwoWindingsTransformerField.setNewValue((TwoWindingsTransformer) equipment, equipmentField, newValue);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.MODIFICATION_ERROR,
                            "Unsupported setting value for equipment type : " + equipment.getType().name());
        }
    }
}
