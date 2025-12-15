/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.DoubleModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IAttributeModificationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.IntegerModificationEmbedded;
import org.gridsuite.modification.server.entities.equipment.modification.attribute.StringModificationEmbedded;

import java.util.Collections;
import java.util.List;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class CurrentTemporaryLimitModificationEmbeddable {

    @Column(name = "name")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "name")),
        @AttributeOverride(name = "opType", column = @Column(name = "nameOp"))
    })
    private StringModificationEmbedded name;

    @Column(name = "value_")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "value_")),
        @AttributeOverride(name = "opType", column = @Column(name = "valueOp"))
    })
    private DoubleModificationEmbedded value;

    @Column(name = "acceptableDuration")
    @Embedded
    @AttributeOverrides(value = {
        @AttributeOverride(name = "value", column = @Column(name = "acceptableDuration")),
        @AttributeOverride(name = "opType", column = @Column(name = "acceptableDurationOp"))
    })
    private IntegerModificationEmbedded acceptableDuration;

    @Column(name = "modificationType")
    @Enumerated(EnumType.STRING)
    private TemporaryLimitModificationType modificationType;

    public static List<CurrentTemporaryLimitModificationEmbeddable> toEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationInfos> limits) {
        if (limits == null) {
            return Collections.emptyList();
        }
        return limits.stream()
                .map(limit -> {
                    return new CurrentTemporaryLimitModificationEmbeddable(
                            new StringModificationEmbedded(limit.getName()),
                            new DoubleModificationEmbedded(limit.getValue()),
                            new IntegerModificationEmbedded(limit.getAcceptableDuration()),
                            limit.getModificationType());
                }).toList();
    }

    public static List<CurrentTemporaryLimitModificationInfos> fromEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationEmbeddable> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitModificationInfos(
                                IAttributeModificationEmbeddable.toAttributeModification(limit.getName()),
                                IAttributeModificationEmbeddable.toAttributeModification(limit.getValue()),
                                IAttributeModificationEmbeddable.toAttributeModification(limit.getAcceptableDuration()),
                                limit.getModificationType()))
                        .toList();
    }
}
