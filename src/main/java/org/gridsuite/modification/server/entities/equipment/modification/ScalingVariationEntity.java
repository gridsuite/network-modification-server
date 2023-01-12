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
import org.hibernate.Hibernate;

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
        assignAttributes(variationInfos);
    }

    public ScalingVariationEntity update(ScalingVariationInfos variationInfos) {
        assignAttributes(variationInfos);
        return this;
    }

    private void assignAttributes(ScalingVariationInfos variationInfos) {
        this.filters = getFiltersEntity(variationInfos);
        this.variationMode = variationInfos.getVariationMode();
        this.variationValue = variationInfos.getVariationValue();
    }

    private List<VariationFilterEntity> getFiltersEntity(ScalingVariationInfos variationInfos) {
        var filters = getFilters();
        return variationInfos.getFilters()
                .stream()
                .map(filterInfos -> {
                    if (filters == null) {
                        return new VariationFilterEntity(filterInfos);
                    } else {
                        return getFilters()
                                .stream()
                                .filter(filter -> Objects.equals(filter.getFilterId(), filterInfos.getId()))
                                .findFirst()
                                .orElse(filterInfos.toEntity());
                    }
                })
                .collect(Collectors.toList());
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

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || Hibernate.getClass(this) != Hibernate.getClass(o)) {
            return false;
        }
        ScalingVariationEntity that = (ScalingVariationEntity) o;
        return id != null && Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
