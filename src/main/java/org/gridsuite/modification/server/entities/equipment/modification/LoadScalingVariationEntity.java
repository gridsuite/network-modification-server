/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
@Entity
@Table(name = "LoadScalingVariation")
public class LoadScalingVariationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL)
    private List<VariationFilterEntity> filters;

    @Column(name = "variationValue")
    private double variationValue;

    @Column(name = "activeVariationMode")
    @Enumerated(EnumType.STRING)
    private VariationMode activeVariationMode;

    @Column(name = "reactiveVariationMode")
    @Enumerated(EnumType.STRING)
    private ReactiveVariationMode reactiveVariationMode;

    public LoadScalingVariation toLoadScalingVariation() {
        return LoadScalingVariation.builder()
                .variationValue(getVariationValue())
                .activeVariationMode(getActiveVariationMode())
                .reactiveVariationMode(getReactiveVariationMode())
                .filters(this.getFilters().stream()
                        .map(filter -> new FilterInfos(filter.getFilterId().toString(), filter.getName()))
                        .collect(Collectors.toList()))
                .build();
    }
}
