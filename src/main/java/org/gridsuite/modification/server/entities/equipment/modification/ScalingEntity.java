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
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@MappedSuperclass
public class ScalingEntity extends ModificationEntity {

    @Column(name = "VariationType")
    @Enumerated(EnumType.STRING)
    private VariationType variationType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<ScalingVariationEntity> variations;

    public ScalingEntity(ScalingInfos scalingInfos) {
        super(scalingInfos);
        assignAttributes(scalingInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttribute((ScalingInfos) modificationInfos);
    }

    private void assignAttribute(ScalingInfos scalingInfos) {
        variationType = scalingInfos.getVariationType();
        if (variations == null) {
            variations = scalingInfos.getVariations().stream().map(ScalingVariationInfos::toEntity).collect(Collectors.toList());
        } else {
            variations.clear();
            variations.addAll(scalingInfos.getVariations().stream().map(ScalingVariationInfos::toEntity).collect(Collectors.toList()));
        }
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
