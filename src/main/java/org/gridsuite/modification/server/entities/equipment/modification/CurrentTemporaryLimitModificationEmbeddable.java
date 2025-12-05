/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import java.util.Collections;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class CurrentTemporaryLimitModificationEmbeddable {

    @Column(name = "name")
    private String name;

    @Column(name = "nameOp")
    @Enumerated(EnumType.STRING)
    private OperationType nameOp;

    @Column(name = "value_")
    private Double value;

    @Column(name = "valueOp")
    @Enumerated(EnumType.STRING)
    private OperationType valueOp;

    @Column(name = "acceptableDuration")
    private Integer acceptableDuration;

    @Column(name = "acceptableDurationOp")
    @Enumerated(EnumType.STRING)
    private OperationType acceptableDurationOp;

    @Column(name = "modificationType")
    @Enumerated(EnumType.STRING)
    private TemporaryLimitModificationType modificationType;

    public static List<CurrentTemporaryLimitModificationEmbeddable> toEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationInfos> limits) {
        if (limits == null) {
            return Collections.emptyList();
        }
        return limits.stream()
                .map(limit -> {
                    String nameValue = limit.getName() != null ? limit.getName().getValue() : null;
                    OperationType nameOp = limit.getName() != null ? limit.getName().getOp() : null;
                    Double valueValue = limit.getValue() != null ? limit.getValue().getValue() : null;
                    OperationType valueOp = limit.getValue() != null ? limit.getValue().getOp() : null;
                    Integer durationValue = limit.getAcceptableDuration() != null ? limit.getAcceptableDuration().getValue() : null;
                    OperationType durationOp = limit.getAcceptableDuration() != null ? limit.getAcceptableDuration().getOp() : null;
                    return new CurrentTemporaryLimitModificationEmbeddable(
                            nameValue,
                            nameOp,
                            valueValue,
                            valueOp,
                            durationValue,
                            durationOp,
                            limit.getModificationType());
                }).toList();
    }

    public static List<CurrentTemporaryLimitModificationInfos> fromEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationEmbeddable> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitModificationInfos(
                                AttributeModification.toAttributeModification(limit.getName(), limit.getNameOp()),
                                AttributeModification.toAttributeModification(limit.getValue(), limit.getValueOp()),
                                AttributeModification.toAttributeModification(limit.getAcceptableDuration(), limit.getAcceptableDurationOp()),
                                limit.getModificationType()))
                        .toList();
    }
}
