/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.dto.LimitsPropertyInfos;

import java.util.UUID;

/**
 * @author El Cheikh Bassel <bassel.el-cheikh_externe at rte-france.com>
 */

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "limits_property")
public class LimitsPropertyEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "value_", nullable = false)
    private String value;

    public static LimitsPropertyEntity fromLimitsPropertyInfos(LimitsPropertyInfos propertyInfos) {
        return new LimitsPropertyEntity(null, propertyInfos.name(), propertyInfos.value());
    }

    public LimitsPropertyInfos toLimitsPropertyInfos() {
        return new LimitsPropertyInfos(name, value);
    }
}
