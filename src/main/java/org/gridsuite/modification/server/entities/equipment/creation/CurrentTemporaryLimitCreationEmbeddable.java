/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.creation;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.CurrentTemporaryLimitCreationInfos;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class CurrentTemporaryLimitCreationEmbeddable {

    @Column(name = "name")
    private String name;

    @Column(name = "value_")
    private Double value;

    @Column(name = "acceptableDuration")
    private Integer acceptableDuration;

    public static List<CurrentTemporaryLimitCreationEmbeddable> toEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitCreationInfos> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitCreationEmbeddable(limit.getName(), limit.getValue(), limit.getAcceptableDuration()))
                        .collect(Collectors.toList());
    }

    public static List<CurrentTemporaryLimitCreationInfos> fromEmbeddableCurrentTemporaryLimits(List<CurrentTemporaryLimitCreationEmbeddable> limits) {
        return limits == null ? null :
                limits.stream()
                        .map(limit -> new CurrentTemporaryLimitCreationInfos(limit.getName(), limit.getValue(), limit.getAcceptableDuration()))
                        .collect(Collectors.toList());
    }

    public CurrentTemporaryLimitCreationInfos toModificationInfos() {
        return CurrentTemporaryLimitCreationInfos.builder()
                .name(getName())
                .value(getValue())
                .acceptableDuration(getAcceptableDuration())
                .build();
    }
}
