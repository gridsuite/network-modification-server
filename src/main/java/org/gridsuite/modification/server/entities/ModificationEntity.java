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
import java.util.Collections;
import java.util.Set;
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
@Setter
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification", indexes = {@Index(name = "modificationEntity_group_id_index", columnList = "group_id")})
public class ModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "group_id_fk_constraint"))
    @Setter
    private ModificationGroupEntity group;

    @Column(name = "date")
    private ZonedDateTime date;

    @Column(name = "type")
    private String type;

    @Column(name = "active", nullable = false)
    private boolean active;

    protected ModificationEntity(ModificationType type) {
        this.id = null;
        this.date = ZonedDateTime.now(ZoneOffset.UTC);
        this.type = type.name();
        this.active = true;
    }

    public ModificationInfos toModificationInfos() {
        return toModificationInfos(Collections.emptySet());
    }

    public ModificationInfos toModificationInfos(Set<String> substationsIds) {
        return ModificationInfos.builder()
                .uuid(this.id)
                .date(this.date)
                .type(ModificationType.valueOf(this.type))
                .substationIds(substationsIds)
                .active(this.active)
                .build();
    }
}
