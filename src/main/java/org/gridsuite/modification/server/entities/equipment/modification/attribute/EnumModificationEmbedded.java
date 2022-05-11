/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import lombok.Data;
import org.gridsuite.modification.server.dto.OperationType;

import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@Embeddable
@Data
public class EnumModificationEmbedded<T extends Enum<?>> implements IAttributeModificationEmbeddable<T> {

    @Enumerated(EnumType.STRING)
    private T value;
    @Enumerated(EnumType.STRING)
    private OperationType opType;

    public EnumModificationEmbedded(org.gridsuite.modification.server.dto.AttributeModification<T> attributeModification) {
        this.value = attributeModification != null ? attributeModification.getValue() : null;
        this.opType = attributeModification != null ? attributeModification.getOp() : null;
    }

    public EnumModificationEmbedded() {
    }
}
