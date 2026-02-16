/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
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
import org.gridsuite.modification.server.dto.catalog.TemporaryLimitInfos;

import java.util.UUID;

/**
 * @author Etienne Lesot <etienne.lesot at rte-france.com>
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Entity
@Table(name = "temporary_limit_for_line_catalog")
public class TemporaryLimitEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column
    private Double limitValue;

    @Column
    private Integer acceptableDuration;

    @Column
    private String name;

    public TemporaryLimitInfos toTemporaryLimitInfos() {
        return TemporaryLimitInfos.builder()
                .id(id)
                .limitValue(limitValue)
                .acceptableDuration(acceptableDuration)
                .name(name)
                .build();
    }
}
