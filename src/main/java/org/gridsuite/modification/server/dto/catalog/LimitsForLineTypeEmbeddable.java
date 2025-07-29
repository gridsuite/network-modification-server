/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.server.dto.catalog;

import jakarta.persistence.Column;
import jakarta.persistence.Embeddable;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Embeddable
public class LimitsForLineTypeEmbeddable {
    @Column
    private String limitSetName;

    @Column
    private Double permanentLimit;

    @Column
    private Double temporaryLimit;

    public static List<LimitsForLineTypeEmbeddable> toEmbeddableLimits(
        List<LimitsForLineTypeInfos> lineTypeInfos) {
        return Objects.isNull(lineTypeInfos) || CollectionUtils.isEmpty(lineTypeInfos) ? null
            : lineTypeInfos.stream()
            .map(typeInfos -> new LimitsForLineTypeEmbeddable(typeInfos.getLimitSetName(),
                typeInfos.getPermanentLimit(), typeInfos.getTemporaryLimit()))
            .toList();
    }
}
