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
import org.gridsuite.modification.dto.CurrentTemporaryLimitModificationInfos;
import org.gridsuite.modification.dto.TemporaryLimitModificationType;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;

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

    @Column(name = "value_")
    private Double value;

    @Column(name = "acceptableDuration")
    private Integer acceptableDuration;

    @Column(name = "modificationType")
    @Enumerated(EnumType.STRING)
    private TemporaryLimitModificationType modificationType;

    public static List<CurrentTemporaryLimitModificationEmbeddable> toEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationInfos> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitModificationEmbeddable(limit.getName(), limit.getValue(), limit.getAcceptableDuration(), limit.getModificationType()))
                        .collect(Collectors.toList());
    }

    public static List<CurrentTemporaryLimitModificationInfos> fromEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitModificationEmbeddable> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitModificationInfos(limit.getName(), limit.getValue(), limit.getAcceptableDuration(), limit.getModificationType()))
                        .collect(Collectors.toList());
    }
}
