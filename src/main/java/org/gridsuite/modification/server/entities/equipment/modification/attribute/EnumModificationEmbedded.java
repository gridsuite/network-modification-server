/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;

import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

/**
 * @author Jacques Borsenberger <jacques.borsenberger at rte-france.com>
 */

@Embeddable
@EqualsAndHashCode
@Getter
public class EnumModificationEmbedded<T extends Enum<?>> implements IAttributeModificationEmbeddable<T> {

    @Enumerated(EnumType.STRING)
    private T value;
    @Enumerated(EnumType.STRING)
    private OperationType opType;

    public EnumModificationEmbedded(AttributeModification<T> attributeModification) {
        this.value = attributeModification != null ? attributeModification.getValue() : null;
        this.opType = attributeModification != null ? attributeModification.getOp() : null;
    }

    public EnumModificationEmbedded() {
    }
}
