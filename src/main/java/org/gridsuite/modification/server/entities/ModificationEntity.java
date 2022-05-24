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

import lombok.EqualsAndHashCode;
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
@EqualsAndHashCode
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification")
public class ModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "date")
    private ZonedDateTime date;

    @Column(name = "type")
    private String type;

    @JoinColumn(name = "groupId", foreignKey = @ForeignKey(name = "group_id_fk_constraint"))
    @ManyToOne(fetch = FetchType.LAZY)
    @Setter
    private ModificationGroupEntity group;

    protected ModificationEntity(ModificationType type) {
        this.id = null;
        this.date = ZonedDateTime.now(ZoneOffset.UTC);
        this.type = type.name();
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
                .build();
    }

    //From https://vladmihalcea.com/the-best-way-to-map-a-onetomany-association-with-jpa-and-hibernate/
    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ModificationEntity)) {
            return false;
        }
        return id != null && id.equals(((ModificationEntity) o).getId());
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }
}
