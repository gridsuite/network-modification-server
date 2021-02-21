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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

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

    @Column(name = "equipmentName")
    private String equipmentName;

    @Column(name = "equipmentAttributeName")
    private String equipmentAttributeName;

    @Column(name = "equipmentAttributeValue")
    private String equipmentAttributeValue;

    public ElementaryModificationEntity(String equipmentId, String equipmentName, String equipmentAttributeName, String equipmentAttributeValue) {
        this.date = ZonedDateTime.now(ZoneOffset.UTC);
        this.type = ModificationType.ELEMENTARY.name();
        this.equipmentId = equipmentId;
        this.equipmentName = equipmentName;
        this.equipmentAttributeName = equipmentAttributeName;
        this.equipmentAttributeValue = equipmentAttributeValue;
    }

    public ElementaryModificationInfos toElementaryModificationInfos() {
        return ElementaryModificationInfos.builder()
                .id(this.id)
                .date(this.date)
                .type(ModificationType.valueOf(this.type))
                .equipmentId(this.equipmentId)
                .equipmentName(this.equipmentName)
                .equipmentAttributeName(this.equipmentAttributeName)
                .equipmentAttributeValue(this.equipmentAttributeValue)
                .build();
    }
}
