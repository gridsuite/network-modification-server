/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import jakarta.persistence.Column;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.MappedSuperclass;

import java.util.HashMap;
import java.util.Map;

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

    @Column(name = "equipmentType", columnDefinition = "varchar(255)")
    @Enumerated(EnumType.STRING)
    private IdentifiableType equipmentType;

    public EquipmentAttributeModificationEntity(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        super(equipmentAttributeModificationInfos);
        assignAttributes(equipmentAttributeModificationInfos);
    }

    @Override
    public void update(ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((EquipmentAttributeModificationInfos) modificationInfos);
    }

    @Override
    public void getAdditionalInfosForMetadata(ModificationInfos modificationInfos) {
        super.getAdditionalInfosForMetadata(modificationInfos);
        try {
            Map<String, String> messageValuesMap = new HashMap<>();
            attributeName = ((EquipmentAttributeModificationInfos) modificationInfos).getEquipmentAttributeName();
            attributeValue = convertAttributeValue(((EquipmentAttributeModificationInfos) modificationInfos).getEquipmentAttributeValue());
            messageValuesMap.put("equipmentAttributeName", attributeName);
            messageValuesMap.put("equipmentId", ((EquipmentAttributeModificationInfos) modificationInfos).getEquipmentId());
            if (attributeValue != null) {
                messageValuesMap.put("equipmentAttributeValue", attributeValue.toString());
            }
            ObjectMapper objectMapper = new ObjectMapper();
            this.setMessageValues(objectMapper.writeValueAsString(messageValuesMap));
        } catch (JsonProcessingException e) {
            e.printStackTrace();
        }
    }

    private void assignAttributes(EquipmentAttributeModificationInfos equipmentAttributeModificationInfos) {
        attributeName = equipmentAttributeModificationInfos.getEquipmentAttributeName();
        attributeValue = convertAttributeValue(equipmentAttributeModificationInfos.getEquipmentAttributeValue());
        equipmentType = equipmentAttributeModificationInfos.getEquipmentType();
    }

    // Override it if you need a special behaviour
    protected T convertAttributeValue(Object attributeValue) {
        return (T) attributeValue;
    }

    @Override
    public EquipmentAttributeModificationInfos toModificationInfos() {
        return toModificationInfosBuilder().build();
    }

    private EquipmentAttributeModificationInfos.EquipmentAttributeModificationInfosBuilder<?, ?> toModificationInfosBuilder() {
        return EquipmentAttributeModificationInfos
            .builder()
            .uuid(getId())
            .date(getDate())
            .equipmentId(getEquipmentId())
            .equipmentAttributeName(getAttributeName())
            .equipmentAttributeValue(getAttributeValue())
            .equipmentType(getEquipmentType());
    }
}
