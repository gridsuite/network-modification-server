/**
 *  Copyright (c) 2023, RTE (http://www.rte-france.com)
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  License, v. 2.0. If a copy of the MPL was not distributed with this
 *  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.UUID;
import org.gridsuite.modification.dto.FreePropertyInfos;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Getter
@NoArgsConstructor
@SuperBuilder
@Entity
@Table(name = "free_property", indexes = @Index(name = "modification_idx", columnList = "equipment_modification_id"))
public class FreePropertyEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "value_")
    private String value;

    @Column(name = "deletion_mark")
    private Boolean deletionMark = false;

    @Column(name = "added")
    private Boolean added = false;

    @Column(name = "previous_value")
    private String previousValue;

    public FreePropertyInfos toInfos() {
        return FreePropertyInfos.builder()
            .name(name)
            .value(value)
            .deletionMark(deletionMark)
            .added(added)
            .previousValue(previousValue)
            .build();
    }
}
