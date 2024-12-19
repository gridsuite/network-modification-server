/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.dto.ReactiveCapabilityCurvePointsInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ReactiveCapabilityCurveCreationEmbeddable;
import org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
public final class DTOUtils {
    private DTOUtils() {
    }

    public static List<ReactiveCapabilityCurvePointsInfos> convertToReactiveCapabilityCurveCreationInfos(List<ReactiveCapabilityCurveCreationEmbeddable> rCCpoints) {
        return CollectionUtils.isEmpty(rCCpoints) ? null : rCCpoints
                .stream()
                .map(value -> new ReactiveCapabilityCurvePointsInfos(value.getMinQ(),
                        value.getMaxQ(),
                        value.getP()))
                .toList();
    }

    public static List<ReactiveCapabilityCurvePointsInfos> convertToReactiveCapabilityCurveModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        return CollectionUtils.isEmpty(rCCpoints) ? null : rCCpoints
                .stream()
                .map(value -> new ReactiveCapabilityCurvePointsInfos(value.getMinQ(),
                        value.getMaxQ(),
                        value.getP()))
                .toList();
    }
}
