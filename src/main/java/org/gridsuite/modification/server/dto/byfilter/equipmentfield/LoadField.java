/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.LoadType;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum LoadField {
    LOAD_TYPE,
    ACTIVE_POWER,
    REACTIVE_POWER;

    public static Object getReferenceValue(Load load, String loadField) {
        LoadField field = LoadField.valueOf(loadField);
        return switch (field) {
            case LOAD_TYPE -> load.getLoadType();
            case ACTIVE_POWER -> load.getP0();
            case REACTIVE_POWER -> load.getQ0();
        };
    }

    public static <T> void setNewValue(Load load, String loadField, T newValue) {
        LoadField field = LoadField.valueOf(loadField);
        switch (field) {
            case LOAD_TYPE -> load.setLoadType(newValue != null ? LoadType.valueOf((String) newValue) : null);
            case ACTIVE_POWER -> load.setP0((double) newValue);
            case REACTIVE_POWER -> load.setQ0((double) newValue);
        }
    }
}
