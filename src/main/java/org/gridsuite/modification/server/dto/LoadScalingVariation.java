/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.entities.equipment.modification.LoadScalingVariationEntity;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@SuperBuilder
@NoArgsConstructor
@Getter
@Setter
@ToString(callSuper = true)
@Schema(description = "Load scaling variation")
public class LoadScalingVariation {
    List<FilterInfos> filters;
    VariationMode activeVariationMode;
    ReactiveVariationMode reactiveVariationMode;
    Double variationValue;

    public LoadScalingVariationEntity toEntity() {
        return LoadScalingVariationEntity.builder()
                .variationValue(getVariationValue())
                .activeVariationMode(getActiveVariationMode())
                .reactiveVariationMode(getReactiveVariationMode())
                .filterIds(getFilters().stream().map(FilterInfos::getId).collect(Collectors.toList()))
                .build();
    }
}
