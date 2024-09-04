/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.byfilter.simple.AbstractSimpleModificationByFilterInfos;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_ERROR;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum LoadField {
    LOAD_TYPE,
    ACTIVE_POWER,
    REACTIVE_POWER;

    public static final String UNSUPPORTED_LOAD_FIELD_ERROR_MESSAGE = "Unsupported load field: ";
    public static final String UNSUPPORTED_LOAD_DATA_TYPE_ERROR_MESSAGE = "Unsupported load data type: ";

    public static Double getReferenceValue(Load load, String loadField) {
        LoadField field = LoadField.valueOf(loadField);
        return switch (field) {
            case ACTIVE_POWER -> load.getP0();
            case REACTIVE_POWER -> load.getQ0();
            default -> throw new NetworkModificationException(MODIFICATION_ERROR, UNSUPPORTED_LOAD_FIELD_ERROR_MESSAGE + field);
        };
    }

    public static void setNewValue(Load load, String loadField, Double newValue) {
        LoadField field = LoadField.valueOf(loadField);
        switch (field) {
            case ACTIVE_POWER -> load.setP0(newValue);
            case REACTIVE_POWER -> load.setQ0(newValue);
            default -> throw new NetworkModificationException(MODIFICATION_ERROR, UNSUPPORTED_LOAD_FIELD_ERROR_MESSAGE + field);
        }
    }

    public static void setNewValue(Load load, String loadField, String newValue) {
        LoadField field = LoadField.valueOf(loadField);
        switch (field) {
            case LOAD_TYPE -> load.setLoadType(newValue != null ? LoadType.valueOf(newValue) : null);
            default -> throw new NetworkModificationException(MODIFICATION_ERROR, UNSUPPORTED_LOAD_FIELD_ERROR_MESSAGE + field);
        }
    }

    public static void setNewValue(Load load, AbstractSimpleModificationByFilterInfos<?> simpleModificationByFilterInfos) {
        switch (simpleModificationByFilterInfos.getDataType()) {
            case DOUBLE, INTEGER -> setNewValue(load, simpleModificationByFilterInfos.getEditedField(), (Double) simpleModificationByFilterInfos.getValue());
            case ENUM -> setNewValue(load, simpleModificationByFilterInfos.getEditedField(), (String) simpleModificationByFilterInfos.getValue());
            default -> throw new NetworkModificationException(MODIFICATION_ERROR, UNSUPPORTED_LOAD_DATA_TYPE_ERROR_MESSAGE + simpleModificationByFilterInfos.getDataType());
        }
    }
}
