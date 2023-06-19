/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.dto.LoadScalingInfos;

import javax.persistence.Entity;
import javax.persistence.Table;
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
        return LoadScalingInfos
                .builder()
                .uuid(getId())
                .groupUuid(getGroup() != null ? getGroup().getId() : null)
                .date(getDate())
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(ScalingVariationEntity::toScalingVariationInfos)
                        .collect(Collectors.toList()))
                .build();
    }
}
