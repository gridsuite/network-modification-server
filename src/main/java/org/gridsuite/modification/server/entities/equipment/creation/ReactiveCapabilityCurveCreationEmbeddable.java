/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.creation;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.dto.ReactiveCapabilityCurveCreationInfos;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ReactiveCapabilityCurveCreationEmbeddable {
    @Column
    private Double minQ;

    @Column
    private Double maxQ;

    @Column
    private Double p;

    public static List<ReactiveCapabilityCurveCreationEmbeddable> toEmbeddableReactiveCapabilityCurve(List<ReactiveCapabilityCurveCreationInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveCreationEmbeddable(point.getMinQ(), point.getMaxQ(), point.getP()))
                .toList();
    }

    public static List<ReactiveCapabilityCurveCreationInfos> toReactiveCapabilityCurveCreationInfos(List<ReactiveCapabilityCurveCreationEmbeddable> points) {
        return CollectionUtils.isEmpty(points) ? null : points
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getMinQ(), value.getMaxQ(), value.getP()))
                .toList();
    }
}
