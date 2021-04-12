/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import java.util.Set;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@Entity
@Table(name = "elementaryModification")
public abstract class AbstractElementaryModificationEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Column(name = "attributeName")
    private String attributeName;

    public abstract Object getAttributeValue();

    @Transient
    private Set<String> substationIds = Set.of();

    public AbstractElementaryModificationEntity(String equipmentId, Set<String> substationId, String attributeName) {
        super(ModificationType.ELEMENTARY);
        this.equipmentId = equipmentId;
        this.substationIds = substationId;
        this.attributeName = attributeName;
    }

    public ElementaryModificationInfos toElementaryModificationInfos() {
        return ElementaryModificationInfos
                .builder()
                .uuid(getUuid())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .substationIds(getSubstationIds())
                .equipmentAttributeName(getAttributeName())
                .equipmentAttributeValue(getAttributeValue())
                .build();
    }
}
