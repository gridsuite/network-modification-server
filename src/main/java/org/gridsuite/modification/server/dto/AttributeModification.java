/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.Objects;

/**
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Attribute modification")
public class AttributeModification<T> {
    T value;
    OperationType op;

    public static <V> AttributeModification<V> toAttributeModification(V value, OperationType opType) {
        if (Objects.isNull(value) && Objects.isNull(opType)) {
            return null;
        } else {
            return new AttributeModification<>(value, opType);
        }
    }

    public T applyModification(T initialValue) {
        if (op == OperationType.SET) {
            return value;
        } else if (op == OperationType.UNSET) {
            return null;
        }
        // TODO throw exception
        return initialValue;
    }
}
