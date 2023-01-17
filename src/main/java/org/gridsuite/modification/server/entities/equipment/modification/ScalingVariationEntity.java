/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "ScalingVariation")
public class ScalingVariationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "variationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    public ScalingVariationEntity(ScalingVariationInfos variationInfos) {
        this.id = null;
        assignAttributes(variationInfos);
    }

    private void assignAttributes(ScalingVariationInfos variationInfos) {
        if (filters == null) {
            this.filters = variationInfos.getFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList());
        } else {
            filters.clear();
            filters.addAll(variationInfos.getFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList()));
        }
        this.variationMode = variationInfos.getVariationMode();
        this.variationValue = variationInfos.getVariationValue();
    }

    public ScalingVariationInfos toScalingVariationInfo() {
        return ScalingVariationInfos.builder()
                .id(getId())
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
