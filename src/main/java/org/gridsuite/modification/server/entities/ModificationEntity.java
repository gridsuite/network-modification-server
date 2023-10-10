/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;

import jakarta.persistence.*;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MISSING_MODIFICATION_DESCRIPTION;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Setter
@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@Table(name = "modification")
public class ModificationEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @Column(name = "date")
    private ZonedDateTime date;

    @JoinColumn(name = "groupId", foreignKey = @ForeignKey(name = "group_id_fk_constraint"))
    @ManyToOne(fetch = FetchType.LAZY)
    @Setter
    private ModificationGroupEntity group;

    @JoinColumn(name = "tabularModificationId", foreignKey = @ForeignKey(name = "tabular_modification_id_fk_constraint"))
    @ManyToOne(fetch = FetchType.LAZY)
    @Setter
    private TabularModificationEntity tabularModification;

    @Column(name = "stashed")
    private Boolean stashed;

    @Column(name = "modifications_order")
    private int modificationsOrder;

    public ModificationEntity(UUID id, ZonedDateTime date, Boolean stashed) {
        this.id = id;
        this.date = date;
        this.stashed = stashed;
    }

    protected ModificationEntity(ModificationInfos modificationInfos) {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MISSING_MODIFICATION_DESCRIPTION, "Missing network modification description");
        }
        //We need to limit the precision to avoid database precision storage limit issue (postgres has a precision of 6 digits while h2 can go to 9)
        this.date = ZonedDateTime.now(ZoneOffset.UTC).truncatedTo(ChronoUnit.MICROS);
    }

    protected ModificationEntity(ModificationInfos modificationInfos, TabularModificationEntity tabularModification) {
        if (modificationInfos == null) {
            throw new NetworkModificationException(MISSING_MODIFICATION_DESCRIPTION, "Missing network modification description");
        }
        //We need to limit the precision to avoid database precision storage limit issue (postgres has a precision of 6 digits while h2 can go to 9)
        this.date = ZonedDateTime.now(ZoneOffset.UTC).truncatedTo(ChronoUnit.MICROS);
        this.tabularModification = tabularModification;
    }

    public ModificationInfos toModificationInfos() {
        return ModificationInfos.builder()
                .uuid(this.id)
                .date(this.date)
                .stashed(this.stashed)
                .build();
    }

    public void update(ModificationInfos modificationInfos) {
        // Basic attributes are immutable in the database
        if (modificationInfos == null) {
            throw new NullPointerException("Impossible to update entity from null DTO");
        }
    }

    public ModificationEntity copy() {
        return toModificationInfos().toEntity();
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
