package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import lombok.Getter;
import org.gridsuite.modification.server.dto.AttributeModification;
import org.gridsuite.modification.server.dto.OperationType;

import javax.persistence.Embeddable;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;

@Embeddable
@Getter
public class IntegerModificationEmbedded implements IAttributeModificationEmbeddable<Integer> {

    private Integer value;
    @Enumerated(EnumType.STRING)
    private OperationType opType;

    public IntegerModificationEmbedded(AttributeModification<Integer> attributeModification) {
        value = attributeModification != null ? attributeModification.getValue() : null;
        opType = attributeModification != null ? attributeModification.getOp() : null;
    }

    public IntegerModificationEmbedded() {
    }
}
