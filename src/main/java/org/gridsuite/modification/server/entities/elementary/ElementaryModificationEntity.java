/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.elementary;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;

import java.util.Set;

import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class ElementaryModificationEntity<T> extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Column(name = "attributeName")
    private String attributeName;

    @Column(name = "attributeValue")
    private T attributeValue;

    protected ElementaryModificationEntity(String equipmentId, String attributeName, T attributeValue) {
        super(ModificationType.ELEMENTARY);
        this.equipmentId = equipmentId;
        this.attributeName = attributeName;
        this.attributeValue = attributeValue;
    }

    public ElementaryModificationInfos toElementaryModificationInfos() {
        return toModificationInfosBuilder().build();
    }

    public ElementaryModificationInfos toElementaryModificationInfos(Set<String> substationId) {
        return toModificationInfosBuilder().substationIds(substationId).build();
    }

    private ElementaryModificationInfos.ElementaryModificationInfosBuilder<?, ?> toModificationInfosBuilder() {
        return ElementaryModificationInfos
                .builder()
                .uuid(getId())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .equipmentAttributeName(getAttributeName())
                .equipmentAttributeValue(getAttributeValue());
    }
}
