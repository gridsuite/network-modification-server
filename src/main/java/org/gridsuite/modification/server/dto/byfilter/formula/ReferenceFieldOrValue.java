/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.formula;

import com.powsybl.iidm.network.Identifiable;
import lombok.*;
import org.gridsuite.modification.server.NetworkModificationException;

import static org.gridsuite.modification.server.dto.byfilter.equipmentfield.FieldUtils.getFieldValue;


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

        String referenceValue = getFieldValue(identifiable, equipmentField);

        if (referenceValue == null) {
            return Double.NaN;
        }

        return Double.parseDouble(referenceValue);
    }

}
