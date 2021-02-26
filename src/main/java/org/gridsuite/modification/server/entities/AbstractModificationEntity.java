/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ModificationInfos;

import javax.persistence.*;
import java.time.ZonedDateTime;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification")
public abstract class AbstractModificationEntity {

    public static final String DATE_COLUMN_NAME = "date";

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    protected UUID uuid;

    @Column(name = DATE_COLUMN_NAME)
    protected ZonedDateTime date;

    @Column(name = "type")
    protected String type;

    public ModificationInfos toModificationInfos() {
        return ModificationInfos.builder()
                .uuid(this.uuid)
                .date(this.date)
                .type(ModificationType.valueOf(this.type))
                .build();
    }
}
