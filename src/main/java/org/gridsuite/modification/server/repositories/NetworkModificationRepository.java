/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.powsybl.commons.PowsyblException;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.*;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public class NetworkModificationRepository {
    private ModificationGroupRepository modificationGroupRepository;

    private ModificationRepository modificationRepository;

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository, ModificationRepository modificationRepository) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
    }

    @Transactional // To have the 2 create in the same transaction (atomic)
    public ElementaryModificationEntity createElementaryModification(UUID groupUuid, String equipmentId, Set<String> substationId,
                                                                     String attributeName, Object attributeValue) {
        ElementaryModificationEntity elementaryModificationEntity = new ElementaryModificationEntity(equipmentId, substationId, createAttributeEntity(attributeName, attributeValue));
        ModificationGroupEntity modificationGroupEntity = this.modificationGroupRepository.findById(groupUuid).orElse(createModificationGroup(groupUuid));
        elementaryModificationEntity.setGroup(modificationGroupEntity);
        this.modificationRepository.save(elementaryModificationEntity);
        return elementaryModificationEntity;
    }

    private ModificationGroupEntity createModificationGroup(UUID groupUuid) {
        return modificationGroupRepository.save(new ModificationGroupEntity(groupUuid));
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getUuid)
                .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModifications(UUID groupUuid) {
        ModificationGroupEntity group = getModificationGroup(groupUuid);
        return this.modificationRepository.findAll(group.getUuid())
                .stream()
                .map(ModificationEntity::toModificationInfos)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    // Due to the lazy loading done outside transaction (demarcation done in findAllByGroupUuid method)
    public List<ElementaryModificationInfos> getElementaryModifications(UUID groupUuid) {
        ModificationGroupEntity group = getModificationGroup(groupUuid);
        return this.modificationRepository.findAllByGroupUuid(group.getUuid())
                .stream()
                .filter(m -> m.getType().equals(ModificationType.ELEMENTARY.name()))
                .map(ElementaryModificationEntity.class::cast)
                .map(ElementaryModificationEntity::toElementaryModificationInfos)
                .collect(Collectors.toList());
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid) {
        this.modificationRepository.deleteAll(this.modificationRepository.findAllByGroupUuid(groupUuid));
        this.modificationGroupRepository.deleteById(groupUuid);
    }

    @Transactional // To have the find end delete in the same transaction (atomic)
    public void deleteModifications(UUID groupUuid, Set<UUID> uuids) {
        List<ModificationEntity> modifications = this.modificationRepository.findAllByGroupUuid(groupUuid)
                .stream()
                .filter(m -> uuids.contains(m.getUuid()))
                .collect(Collectors.toList());
        this.modificationRepository.deleteAll(modifications);
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }

    private AbstractAttributeEntity createAttributeEntity(String attributeName, Object attributeValue) {
        if (attributeValue.getClass().isEnum()) {
            return new StringAttributeEntity(attributeName, attributeValue.toString());
        } else {
            switch (attributeValue.getClass().getSimpleName()) {
                case "String":
                    return new StringAttributeEntity(attributeName, (String) attributeValue);
                case "Boolean":
                    return new BooleanAttributeEntity(attributeName, (boolean) attributeValue);
                case "Integer":
                    return new IntegerAttributeEntity(attributeName, (int) attributeValue);
                case "Float":
                    return new FloatAttributeEntity(attributeName, (float) attributeValue);
                case "Double":
                    return new DoubleAttributeEntity(attributeName, (double) attributeValue);
                default:
                    throw new PowsyblException("Value type invalid : " + attributeValue.getClass().getSimpleName());
            }
        }
    }
}
