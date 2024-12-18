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
import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class ReactiveCapabilityCurveEmbeddable {
    @Column
    private Double minQ;

    @Column
    private Double maxQ;

    @Column
    private Double p;

    public static List<ReactiveCapabilityCurveEmbeddable> toEmbeddableReactiveCapabilityCurve(List<ReactiveCapabilityCurvePointsInfos> points) {
        return points == null ? null
                : points.stream()
                .map(point -> new ReactiveCapabilityCurveEmbeddable(point.getMinQ(), point.getMaxQ(), point.getP()))
                .toList();
    }

    public static List<ReactiveCapabilityCurvePointsInfos> toReactiveCapabilityCurveCreationInfos(List<ReactiveCapabilityCurveEmbeddable> points) {
        return CollectionUtils.isEmpty(points) ? null : points
                .stream()
                .map(value -> new ReactiveCapabilityCurvePointsInfos(value.getMinQ(), value.getMaxQ(), value.getP()))
                .toList();
    }
}
