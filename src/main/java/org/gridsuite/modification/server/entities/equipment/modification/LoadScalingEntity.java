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
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.*;
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

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
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
        if (variations == null) {
            variations = loadScalingInfos.getVariations().stream().map(ScalingVariationInfos::toEntity).collect(Collectors.toList());
        } else {
            variations.clear();
            variations.addAll(loadScalingInfos.getVariations().stream().map(ScalingVariationInfos::toEntity).collect(Collectors.toList()));
        }
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
                        .map(ScalingVariationEntity::toScalingVariationInfos)
                        .collect(Collectors.toList()))
                .build();
    }
}
