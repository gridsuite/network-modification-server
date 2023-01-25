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
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.Column;
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

    @Column(name = "isIterative")
    private boolean isIterative;

    public GeneratorScalingEntity(@NotNull GeneratorScalingInfos generatorScalingInfos) {
        super(ModificationType.GENERATOR_SCALING);
        assignAttributes(generatorScalingInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((GeneratorScalingInfos) modificationInfos);
    }

    private void assignAttributes(GeneratorScalingInfos generatorScalingInfos) {
        super.assignAttribute(generatorScalingInfos);
        isIterative = generatorScalingInfos.getIsIterative();
    }

    @Override
    public GeneratorScalingInfos toModificationInfos() {
        return GeneratorScalingInfos.builder()
                .type(ModificationType.valueOf(getType()))
                .date(getDate())
                .uuid(getId())
                .isIterative(isIterative())
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(ScalingVariationEntity::toScalingVariationInfos)
                        .collect(Collectors.toList()))
                .build();
    }
}
