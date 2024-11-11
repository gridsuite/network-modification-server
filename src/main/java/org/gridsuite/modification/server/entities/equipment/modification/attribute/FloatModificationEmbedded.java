/*
 *  Copyright (c) 2022, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import lombok.Getter;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.OperationType;

import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

/**
 * @author Ayoub Labidi <ayoub.labidi at rte-france.com>
 */

@Embeddable
@Getter
public class FloatModificationEmbedded implements IAttributeModificationEmbeddable<Float> {

    private Float value;
    @Enumerated(EnumType.STRING)
    private OperationType opType;

    public FloatModificationEmbedded(AttributeModification<Float> attributeModification) {
        value = attributeModification != null ? attributeModification.getValue() : null;
        opType = attributeModification != null ? attributeModification.getOp() : null;
    }

    public FloatModificationEmbedded() {
    }
}
