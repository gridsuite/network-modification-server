/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.entities.equipment.modification.attribute;

import com.powsybl.iidm.network.IdentifiableType;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.lang.reflect.Constructor;

import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.EntityRegistry;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;

import jakarta.persistence.Column;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.MappedSuperclass;


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
            .stashed(getStashed())
            .activated(getActivated())
            .equipmentId(getEquipmentId())
            .equipmentAttributeName(getAttributeName())
            .equipmentAttributeValue(getAttributeValue())
            .equipmentType(getEquipmentType());
    }

    public static EquipmentAttributeModificationEntity<?> createAttributeEntity(EquipmentAttributeModificationInfos dto) {
        Object equipmentAttributeValue = dto.getEquipmentAttributeValue();
        Class<?> attributeValueClass = String.class;
        if (equipmentAttributeValue != null && !equipmentAttributeValue.getClass().isEnum()) {
            attributeValueClass = equipmentAttributeValue.getClass();
        }
        Class<? extends EquipmentAttributeModificationEntity<?>> entityClass = EntityRegistry.getAttributeEntityClass(attributeValueClass);

        if (entityClass != null) {
            try {
                Constructor<? extends EquipmentAttributeModificationEntity<?>> constructor = entityClass.getConstructor(EquipmentAttributeModificationInfos.class);
                return constructor.newInstance(dto);
            } catch (Exception e) {
                throw new RuntimeException("Failed to map DTO to Entity", e);
            }
        } else {
            throw new IllegalArgumentException("No entity class registered for attribute value class: " + attributeValueClass);
        }
    }
}
