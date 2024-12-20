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
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;

import jakarta.persistence.*;
import java.util.List;
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

    @Column(name = "reactiveVariationMode")
    private ReactiveVariationMode reactiveVariationMode;

    public ScalingVariationEntity(ScalingVariationInfos variationInfos) {
        this.id = null;
        assignAttributes(variationInfos);
    }

    private void assignAttributes(ScalingVariationInfos variationInfos) {
        if (filters == null) {
            this.filters = variationInfos.getFilters().stream()
                .map(VariationFilterEntity::new)
                .collect(Collectors.toList());
        } else {
            filters.clear();
            filters.addAll(variationInfos.getFilters().stream()
                .map(VariationFilterEntity::new)
                .collect(Collectors.toList()));
        }
        this.variationMode = variationInfos.getVariationMode();
        this.reactiveVariationMode = variationInfos.getReactiveVariationMode();
        this.variationValue = variationInfos.getVariationValue();
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

}
