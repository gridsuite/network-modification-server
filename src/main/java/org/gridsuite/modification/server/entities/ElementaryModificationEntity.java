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
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;

import javax.persistence.*;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "elementaryModification")
public class ElementaryModificationEntity extends AbstractModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Transient
    private Set<String> substationIds = Set.of();

    @OneToOne(fetch = FetchType.EAGER, optional = false, orphanRemoval = true, cascade = {CascadeType.ALL})
    @JoinColumn(name = "attribute_id")
    private AbstractAttributeEntity attribute;

    public ElementaryModificationEntity(String equipmentId, Set<String> substationId, AbstractAttributeEntity attribute) {
        this.date = ZonedDateTime.now(ZoneOffset.UTC);
        this.type = ModificationType.ELEMENTARY.name();
        this.equipmentId = equipmentId;
        this.substationIds = substationId;
        this.attribute = attribute;
    }

    public ElementaryModificationInfos toElementaryModificationInfos() {
        return ElementaryModificationInfos.builder()
                .uuid(this.uuid)
                .date(this.date)
                .type(ModificationType.valueOf(this.type))
                .equipmentId(this.equipmentId)
                .substationIds(this.substationIds)
                .equipmentAttributeName(this.attribute.getAttributeName())
                .equipmentAttributeValue(this.attribute.getAttributeValue())
                .build();
    }
}
