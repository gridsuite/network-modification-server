/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.equipmentfield;

import com.powsybl.iidm.network.Load;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import static org.gridsuite.modification.server.modifications.LoadModification.modifyP0;
import static org.gridsuite.modification.server.modifications.LoadModification.modifyQ0;
import com.powsybl.iidm.network.LoadType;
import jakarta.validation.constraints.NotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
public enum LoadField {
    LOAD_TYPE,
    ACTIVE_POWER,
    REACTIVE_POWER;

    public static String getReferenceValue(Load load, String loadField) {
        LoadField field = LoadField.valueOf(loadField);
        return switch (field) {
            case LOAD_TYPE -> load.getLoadType().name();
            case ACTIVE_POWER -> String.valueOf(load.getP0());
            case REACTIVE_POWER -> String.valueOf(load.getQ0());
        };
    }

    public static void setNewValue(Load load, String loadField, @NotNull String newValue) {
        LoadField field = LoadField.valueOf(loadField);
        switch (field) {
            case LOAD_TYPE -> load.setLoadType(LoadType.valueOf(newValue));
            case ACTIVE_POWER -> modifyP0(load, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null);
            case REACTIVE_POWER -> modifyQ0(load, new AttributeModification<>(Double.parseDouble(newValue), OperationType.SET), null);
        }
    }
}
