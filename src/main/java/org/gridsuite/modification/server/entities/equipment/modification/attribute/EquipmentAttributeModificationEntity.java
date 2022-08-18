/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import javax.persistence.Column;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.MappedSuperclass;

import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@NoArgsConstructor
@Getter
@MappedSuperclass
public class EquipmentAttributeModificationEntity<T> extends EquipmentModificationEntity {
    @Column(name = "attributeName")
    private String attributeName;

    @Column(name = "attributeValue")
    private T attributeValue;

    @Column(name = "equipmentType")
    @Enumerated(EnumType.STRING)
    private IdentifiableType equipmentType;

    protected EquipmentAttributeModificationEntity(String equipmentId, String attributeName, T attributeValue, IdentifiableType equipmentType) {
        super(equipmentId, ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION);
        this.attributeName = attributeName;
        this.attributeValue = attributeValue;
        this.equipmentType = equipmentType;
    }

    @Override
    public EquipmentAttributeModificationInfos toModificationInfos() {
        return toModificationInfosBuilder().build();
    }

    public EquipmentAttributeModificationInfos toEquipmentAttributeModificationInfos() {
        return toModificationInfosBuilder().build();
    }

    private EquipmentAttributeModificationInfos.EquipmentAttributeModificationInfosBuilder<?, ?> toModificationInfosBuilder() {
        return EquipmentAttributeModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .type(ModificationType.valueOf(getType()))
            .equipmentId(getEquipmentId())
            .equipmentAttributeName(getAttributeName())
            .equipmentAttributeValue(getAttributeValue())
            .equipmentType(getEquipmentType());
    }
}
