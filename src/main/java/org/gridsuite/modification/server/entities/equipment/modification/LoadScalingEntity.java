/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.dto.LoadScalingInfos;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "LoadScaling")
public class LoadScalingEntity extends ScalingEntity {

    public LoadScalingEntity(@NonNull LoadScalingInfos loadScalingInfos) {
        super(loadScalingInfos);
    }

    @Override
    public LoadScalingInfos toModificationInfos() {
        return toLoadScalingInfosBuilder().build();
    }

    private LoadScalingInfos.LoadScalingInfosBuilder<?, ?> toLoadScalingInfosBuilder() {
        return toModificationInfosBuilder(LoadScalingInfos
                .builder()
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(ScalingVariationEntity::toScalingVariationInfos)
                        .collect(Collectors.toList())));
    }
}
