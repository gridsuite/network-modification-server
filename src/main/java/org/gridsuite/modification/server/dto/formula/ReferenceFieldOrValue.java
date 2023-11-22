/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula;

import com.powsybl.iidm.network.Battery;
import com.powsybl.iidm.network.Generator;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Load;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.VoltageLevel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.GeneratorField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.LoadField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.server.dto.formula.equipmentfield.VoltageLevelField;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ReferenceFieldOrValue {
    private String equipmentField;

    private Double value;

    public Double getRefOrValue(Identifiable<?> identifiable) {
        if (value == null && equipmentField == null) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                    "There is no value or reference to any of the equipment fields");
        }

        if (value != null && !Double.isNaN(value)) {
            return value;
        }

        IdentifiableType identifiableType = identifiable.getType();
        Double referenceValue = switch (identifiableType) {
            case GENERATOR -> GeneratorField.getReferenceValue((Generator) identifiable, equipmentField);
            case BATTERY -> BatteryField.getReferenceValue((Battery) identifiable, equipmentField);
            case SHUNT_COMPENSATOR -> ShuntCompensatorField.getReferenceValue((ShuntCompensator) identifiable, equipmentField);
            case VOLTAGE_LEVEL -> VoltageLevelField.getReferenceValue((VoltageLevel) identifiable, equipmentField);
            case LOAD -> LoadField.getReferenceValue((Load) identifiable, equipmentField);
            default -> throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                    String.format("Unsupported equipment type : %s", identifiableType.name()));
        };

        if (referenceValue == null) {
            throw new NetworkModificationException(NetworkModificationException.Type.BY_FORMULA_MODIFICATION_ERROR,
                    String.format("value of %s is null", equipmentField));
        }

        return referenceValue;
    }
}
