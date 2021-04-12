/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import javax.persistence.*;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.UUID;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ModificationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification", indexes = {@Index(name = "modificationEntity_group_uuid_index", columnList = "group_uuid")})
public class ModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "uuid")
    private UUID uuid;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(foreignKey = @ForeignKey(
            name = "group_uuid_fk_constraint"
    ))
    @Setter
    private ModificationGroupEntity group;

    @Column(name = "date")
    private ZonedDateTime date;

    @Column(name = "type")
    private String type;

    protected ModificationEntity(ModificationType type) {
        this.uuid = null;
        this.date = ZonedDateTime.now(ZoneOffset.UTC);
        this.type = type.name();
    }

    public ModificationInfos toModificationInfos() {
        return ModificationInfos.builder()
                .uuid(this.uuid)
                .date(this.date)
                .type(ModificationType.valueOf(this.type))
                .build();
    }
}
