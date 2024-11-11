/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

public interface IAttributeModificationEmbeddable<T> {
    T getValue();

    OperationType getOpType();

    static <U> AttributeModification<U> toAttributeModification(IAttributeModificationEmbeddable<U> val) {
        return val == null ? null : new AttributeModification<>(val.getValue(), val.getOpType());
    }
}
