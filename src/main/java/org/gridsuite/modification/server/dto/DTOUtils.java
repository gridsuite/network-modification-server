/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.model.ReactiveCapabilityCurvePointsModel;
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

    public static List<ReactiveCapabilityCurvePointsModel> toReactiveCapabilityCurvePointsCreationInfos(List<ReactiveCapabilityCurveCreationEmbeddable> rCCpoints) {
        return Objects.isNull(rCCpoints) || CollectionUtils.isEmpty(rCCpoints) ? null
                : rCCpoints.stream().map(point -> new ReactiveCapabilityCurvePointsModel(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .toList();
    }

    public static List<ReactiveCapabilityCurvePointsModel> toReactiveCapabilityCurvePointsModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        return Objects.isNull(rCCpoints) || CollectionUtils.isEmpty(rCCpoints) ? null
                : rCCpoints.stream().map(point -> new ReactiveCapabilityCurvePointsModel(point.getMinQ(),
                        point.getMaxQ(),
                        point.getP()))
                .toList();
    }
}
