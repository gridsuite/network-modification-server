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
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;

import javax.persistence.*;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
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

    @OneToMany(cascade = CascadeType.ALL)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "variationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode variationMode;

    @Column(name = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    public ScalingVariationEntity(ScalingVariationInfos variationInfos) {
        this.id = null;
        assignAttributes(variationInfos);
    }

    private void assignAttributes(ScalingVariationInfos variationInfos) {
        this.filters = getFiltersEntity(variationInfos);
        this.variationMode = variationInfos.getVariationMode();
        this.variationValue = variationInfos.getVariationValue();
        this.reactiveVariationMode = variationInfos.getReactiveVariationMode();
    }

    public ScalingVariationInfos toScalingVariationInfos() {
        return ScalingVariationInfos.builder()
                .id(getId())
                .variationMode(getVariationMode())
                .variationValue(getVariationValue())
                .reactiveVariationMode(getReactiveVariationMode())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }

    public ScalingVariationEntity update(ScalingVariationInfos variation) {
        this.reactiveVariationMode = variation.getReactiveVariationMode();
        this.variationMode = variation.getVariationMode();
        this.variationValue = variation.getVariationValue();
        this.filters = getFiltersEntity(variation);
        return this;
    }

    private List<VariationFilterEntity> getFiltersEntity(ScalingVariationInfos variationInfos) {
        return variationInfos.getFilters()
                .stream()
                .map(filterInfos -> {
                    if (getFilters() == null) {
                        return new VariationFilterEntity(filterInfos);
                    } else {
                        return getFilters()
                                .stream()
                                .filter(filter -> Objects.equals(filter.getFilterId(), filterInfos.getId()))
                                .findFirst()
                                .orElse(new VariationFilterEntity(filterInfos));
                    }
                })
                .collect(Collectors.toList());
    }
}
