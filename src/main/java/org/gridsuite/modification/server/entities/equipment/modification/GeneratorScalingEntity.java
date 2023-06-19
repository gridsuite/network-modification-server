/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Setter
@Entity
@EqualsAndHashCode
@Table(name = "GeneratorScaling")
public class GeneratorScalingEntity extends ScalingEntity {

    public GeneratorScalingEntity(@NotNull GeneratorScalingInfos generatorScalingInfos) {
        super(generatorScalingInfos);
    }

    @Override
    public GeneratorScalingInfos toModificationInfos() {
        return GeneratorScalingInfos.builder()
                .date(getDate())
                .uuid(getId())
                .groupUuid(getGroup().getId())
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(ScalingVariationEntity::toScalingVariationInfos)
                        .collect(Collectors.toList()))
                .build();
    }
}
