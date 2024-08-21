/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Load;
import org.gridsuite.modification.server.dto.byfilter.simple.SimpleModificationByFilterInfos;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public enum LoadField {
    ACTIVE_POWER,
    REACTIVE_POWER;

    public static Double getReferenceValue(Load load, String loadField) {
        LoadField field = LoadField.valueOf(loadField);
        return switch (field) {
            case ACTIVE_POWER -> load.getP0();
            case REACTIVE_POWER -> load.getQ0();
        };
    }

    public static void setNewValue(Load load, String loadField, Double newValue) {
        LoadField field = LoadField.valueOf(loadField);
        switch (field) {
            case ACTIVE_POWER -> load.setP0(newValue);
            case REACTIVE_POWER -> load.setQ0(newValue);
        }
    }

    public static void setNewValue(Load load, SimpleModificationByFilterInfos<?> fieldModificationInfos) {
        switch (fieldModificationInfos.getDataType()) {
            case DOUBLE -> setNewValue(load, fieldModificationInfos.getEditedField(), (Double) fieldModificationInfos.getValue());
        }
    }
}
