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
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.catalog.LimitsForLineTypeInfos;

import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@Getter
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

    @Column
    private Double temporaryLimitValue;

    @Column
    private Integer temporaryLimitAcceptableDuration;

    @Column
    private String temporaryLimitName;

    @Column
    private String area;

    @Column
    private String temperature;

    public LimitsForLineTypeInfos toLineTypeInfos() {
        return LimitsForLineTypeInfos.builder()
            .id(id)
            .limitSetName(limitSetName)
            .permanentLimit(permanentLimit)
            .temporaryLimitValue(temporaryLimitValue)
            .temporaryLimitAcceptableDuration(temporaryLimitAcceptableDuration)
            .temporaryLimitName(temporaryLimitName)
            .area(area)
            .temperature(temperature)
            .build();
    }

    public LimitsForLineTypeEntity(LimitsForLineTypeInfos limitsForLineTypeInfos) {
        this(limitsForLineTypeInfos.getId(),
            limitsForLineTypeInfos.getLimitSetName(),
            limitsForLineTypeInfos.getPermanentLimit(),
            limitsForLineTypeInfos.getTemporaryLimitValue(),
            limitsForLineTypeInfos.getTemporaryLimitAcceptableDuration(),
            limitsForLineTypeInfos.getTemporaryLimitName(),
            limitsForLineTypeInfos.getArea(),
            limitsForLineTypeInfos.getTemperature());
    }
}
