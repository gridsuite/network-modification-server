/**
 *  Copyright (c) 2025, RTE (http://www.rte-france.com)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.dto.TabularPropertyInfos;

import java.util.UUID;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Entity
@Table(name = "tabular_property", indexes = @Index(name = "tabular_modification_idx", columnList = "tabular_modification_id"))
public class TabularPropertyEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "predefined")
    private boolean predefined;

    @Column(name = "selected")
    private boolean selected = false;

    public TabularPropertyInfos toInfos() {
        return TabularPropertyInfos.builder()
            .name(name)
            .predefined(predefined)
            .selected(selected)
            .build();
    }

    public TabularPropertyEntity(TabularPropertyInfos propertyInfos) {
        this(null, propertyInfos.getName(), propertyInfos.isPredefined(), propertyInfos.isSelected());
    }
}
