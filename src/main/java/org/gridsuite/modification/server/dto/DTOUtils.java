/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.dto.ReactiveCapabilityCurveCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ReactiveCapabilityCurveCreationEmbeddable;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
public final class DTOUtils {
    private DTOUtils() {
    }

    public static List<ReactiveCapabilityCurveCreationInfos> convertToReactiveCapabilityCurveModificationInfos(List<ReactiveCapabilityCurveCreationEmbeddable> rCCpoints) {
        return CollectionUtils.isEmpty(rCCpoints) ? null : rCCpoints
                .stream()
                .map(value -> new ReactiveCapabilityCurveCreationInfos(value.getMinQ(), value.getMaxQ(), value.getP()))
                .toList();
    }
}
