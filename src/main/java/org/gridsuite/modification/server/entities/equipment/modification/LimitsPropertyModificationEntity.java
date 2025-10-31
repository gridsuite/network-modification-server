/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
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
@Table(name = "limits_property_modification")
public class LimitsPropertyModificationEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "property_value", nullable = false)
    private String propertyValue;

    public static LimitsPropertyModificationEntity fromLimitsPropertyInfos(LimitsPropertyInfos propertyInfos) {
        return new LimitsPropertyModificationEntity(null, propertyInfos.name(), propertyInfos.value());
    }

    public LimitsPropertyInfos toLimitsPropertyInfos() {
        return new LimitsPropertyInfos(name, propertyValue);
    }
}
