/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.server.dto.catalog.LimitsForLineTypeEmbeddable;
import org.gridsuite.modification.server.dto.catalog.LimitsForLineTypeInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ReactiveCapabilityCurveCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
public final class DTOUtils {
    private DTOUtils() {
    }

    public static List<ReactiveCapabilityCurvePointsInfos> toReactiveCapabilityCurvePointsCreationInfos(List<ReactiveCapabilityCurveCreationEmbeddable> rCCpoints) {
        return Objects.isNull(rCCpoints) || CollectionUtils.isEmpty(rCCpoints) ? null
                : rCCpoints.stream().map(point -> new ReactiveCapabilityCurvePointsInfos(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .toList();
    }

    public static List<ReactiveCapabilityCurvePointsInfos> toReactiveCapabilityCurvePointsModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        return Objects.isNull(rCCpoints) || CollectionUtils.isEmpty(rCCpoints) ? null
                : rCCpoints.stream().map(point -> new ReactiveCapabilityCurvePointsInfos(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .toList();
    }

    public static List<LimitsForLineTypeInfos> toLimitsForLineTypeInfos(List<LimitsForLineTypeEmbeddable> limits) {
        return Objects.isNull(limits) || CollectionUtils.isEmpty(limits) ? null
            : limits.stream().map(limit -> new LimitsForLineTypeInfos(limit.getLimitSetName(),
                limit.getPermanentLimit(),
                limit.getTemporaryLimit()))
            .toList();
    }
}
