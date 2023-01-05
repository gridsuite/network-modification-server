/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Setter
@Entity
@EqualsAndHashCode
@Table(name = "ScalingVariation")
public class ScalingVariationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ManyToMany(cascade = CascadeType.ALL)
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
        this.filters = getFiltersEntity(variationInfos);
        this.variationMode = variationInfos.getVariationMode();
        this.variationValue = variationInfos.getVariationValue();
    }

    private List<VariationFilterEntity> getFiltersEntity(ScalingVariationInfos variationInfos) {
        return variationInfos.getFilters().stream().map(filterInfos -> VariationFilterEntity.builder()
                        .filterId(UUID.fromString(filterInfos.getId()))
                        .name(filterInfos.getName())
                        .build())
                .collect(Collectors.toList());
    }

    public ScalingVariationInfos toScalingVariation() {
        return ScalingVariationInfos.builder()
                .id(getId())
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
