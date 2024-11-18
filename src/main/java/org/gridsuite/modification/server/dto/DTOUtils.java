/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import org.gridsuite.modification.server.entities.equipment.modification.ReactiveCapabilityCurveModificationEmbeddable;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * @author jamal kheyyad <jamal.kheyyad at rte-france.com>
 */
public final class DTOUtils {
    private DTOUtils() {
    }

    public static List<ReactiveCapabilityCurveModificationInfos> convertToReactiveCapabilityCurveModificationInfos(List<ReactiveCapabilityCurveModificationEmbeddable> rCCpoints) {
        return Optional.ofNullable(rCCpoints)
                .orElse(Collections.emptyList()).stream().map(value -> value == null
                        ? new ReactiveCapabilityCurveModificationInfos(null, null, null, null, null, null)
                        : new ReactiveCapabilityCurveModificationInfos(
                        value.getMinQ(),
                        value.getOldMinQ(),
                        value.getMaxQ(),
                        value.getOldMaxQ(),
                        value.getP(),
                        value.getOldP())).toList();
    }
}
