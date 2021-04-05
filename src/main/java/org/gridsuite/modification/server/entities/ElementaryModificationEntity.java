/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities;

import javax.persistence.*;

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
public class ElementaryModificationEntity extends ModificationEntity {
    @Column(name = "equipmentId")
    private String equipmentId;

    @Transient
    private Set<String> substationIds = Set.of();

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, optional = false, orphanRemoval = true)
    @JoinColumn(name = "attribute_id",
            referencedColumnName = "id",
            foreignKey = @ForeignKey(
                    name = "attribute_id_fk_constraint"
            ))
    private AbstractAttributeEntity attribute;

    public ElementaryModificationEntity(String equipmentId, Set<String> substationId, AbstractAttributeEntity attribute) {
        super(ModificationType.ELEMENTARY);
        this.equipmentId = equipmentId;
        this.substationIds = substationId;
        this.attribute = attribute;
    }

    public ElementaryModificationInfos toElementaryModificationInfos() {
        return ElementaryModificationInfos
                .builder()
                .uuid(getUuid())
                .date(getDate())
                .type(ModificationType.valueOf(getType()))
                .equipmentId(getEquipmentId())
                .substationIds(getSubstationIds())
                .equipmentAttributeName(getAttribute().getAttributeName())
                .equipmentAttributeValue(getAttribute().getAttributeValue())
                .build();
    }
}
