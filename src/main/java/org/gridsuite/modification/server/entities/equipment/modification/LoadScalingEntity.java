/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.ArrayList;
import java.util.List;
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

    @OneToMany(cascade = CascadeType.ALL)
    private List<ScalingVariationEntity> variations;

    public LoadScalingEntity(@NonNull LoadScalingInfos loadScalingInfos) {
        super(ModificationType.LOAD_SCALING);
        assignAttributes(loadScalingInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((LoadScalingInfos) modificationInfos);
    }

    private void assignAttributes(LoadScalingInfos loadScalingInfos) {
        setVariationType(loadScalingInfos.getVariationType());
        setVariations(loadScalingInfos.getVariations()
                .stream()
                .map(ScalingVariationInfos::toEntity)
                .collect(Collectors.toList()));
    }

    @Override
    public LoadScalingInfos toModificationInfos() {
        return LoadScalingInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .variationType(getVariationType())
                .variations(getVariations().stream()
                        .map(ScalingVariationEntity::toScalingVariation)
                        .collect(Collectors.toList()))
                .build();
    }

    @Override
    public void cloneWithIdsToNull() {
        setId(null);
        this.variations = getVariations()
                .stream()
                .peek(variation -> {
                    variation.setId(null);
                    variation.setFilters(new ArrayList<>(variation.getFilters()
                            .stream()
                            .peek(filter -> filter.setId(null))
                            .collect(Collectors.toList())));
                }).collect(Collectors.toList());
    }
}
