/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * SPDX-License-Identifier: MPL-2.0
 */
package org.gridsuite.modification.server.entities.catalog;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.catalog.LimitsForLineTypeInfos;
import org.gridsuite.modification.server.dto.catalog.TemporaryLimitInfos;

import java.util.List;
import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Entity
@Table(name = "limits_for_line_type")
public class LimitsForLineTypeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private String limitSetName;

    @Column
    private Double permanentLimit;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "temporary_limit_id", nullable = false)
    private List<TemporaryLimitEntity> temporaryLimits;

    @Column
    private String area;

    @Column
    private String temperature;

    public LimitsForLineTypeInfos toLineTypeInfosWithoutLimits() {
        return LimitsForLineTypeInfos.builder()
                .id(id)
                .area(area)
                .temperature(temperature)
                .build();
    }

    public LimitsForLineTypeInfos toLineTypeInfos() {
        return LimitsForLineTypeInfos.builder()
            .id(id)
            .limitSetName(limitSetName)
            .permanentLimit(permanentLimit)
            .temporaryLimits(temporaryLimits.parallelStream().map(TemporaryLimitEntity::toTemporaryLimitInfos).toList())
            .area(area)
            .temperature(temperature)
            .build();
    }

    public LimitsForLineTypeEntity(LimitsForLineTypeInfos limitsForLineTypeInfos) {
        this(limitsForLineTypeInfos.getId(),
            limitsForLineTypeInfos.getLimitSetName(),
            limitsForLineTypeInfos.getPermanentLimit(),
            limitsForLineTypeInfos.getTemporaryLimits() != null ? limitsForLineTypeInfos.getTemporaryLimits().stream().map(TemporaryLimitInfos::toTemporaryLimitEntity).toList() : null,
            limitsForLineTypeInfos.getArea(),
            limitsForLineTypeInfos.getTemperature());
    }
}
