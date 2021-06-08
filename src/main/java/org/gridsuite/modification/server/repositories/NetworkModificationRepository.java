/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.powsybl.commons.PowsyblException;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.elementary.*;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_GROUP_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.MODIFICATION_NOT_FOUND;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public class NetworkModificationRepository {
    private final ModificationGroupRepository modificationGroupRepository;

    private final ModificationRepository modificationRepository;

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository, ModificationRepository modificationRepository) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
    }

    public <T> ElementaryModificationEntity<T> createElementaryModification(String equipmentId, String attributeName, T attributeValue) {
        ElementaryModificationEntity<?> modification;
        if (attributeValue == null) {
            modification = new StringElementaryModificationEntity(equipmentId, attributeName, null);
        } else if (attributeValue.getClass().isEnum()) {
            modification = new StringElementaryModificationEntity(equipmentId, attributeName, attributeValue.toString());
        } else {
            switch (attributeValue.getClass().getSimpleName()) {
                case "String":
                    modification = new StringElementaryModificationEntity(equipmentId, attributeName, (String) attributeValue);
                    break;
                case "Boolean":
                    modification = new BooleanElementaryModificationEntity(equipmentId, attributeName, (boolean) attributeValue);
                    break;
                case "Integer":
                    modification = new IntegerElementaryModificationEntity(equipmentId, attributeName, (int) attributeValue);
                    break;
                case "Float":
                    modification = new FloatElementaryModificationEntity(equipmentId, attributeName, (float) attributeValue);
                    break;
                case "Double":
                    modification = new DoubleElementaryModificationEntity(equipmentId, attributeName, (double) attributeValue);
                    break;
                default:
                    throw new PowsyblException("Value type invalid : " + attributeValue.getClass().getSimpleName());
            }
        }

        return (ElementaryModificationEntity<T>) modification;
    }

    @Transactional // To have all create in the same transaction (atomic)
    public void saveModifications(UUID groupUuid, List<ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(m -> m.setGroup(modificationGroupEntity));
        this.modificationRepository.saveAll(modifications);
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModifications(UUID groupUuid) {
        ModificationGroupEntity group = getModificationGroup(groupUuid);
        return this.modificationRepository.findAllBaseByGroupId(group.getId())
                .stream()
                .map(ModificationEntity::toModificationInfos)
                .collect(Collectors.toList());
    }

    public ElementaryModificationInfos getElementaryModification(UUID groupUuid, UUID modificationUuid) {
        return ((ElementaryModificationEntity<?>) this.modificationRepository
                .findById(modificationUuid)
                .filter(m -> ModificationType.ELEMENTARY.name().equals(m.getType()))
                .filter(m -> groupUuid.equals(m.getGroup().getId()))
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString())))
                .toElementaryModificationInfos();
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        this.modificationRepository.deleteAll(this.modificationRepository.findAllByGroupId(groupUuid));
        this.modificationGroupRepository.delete(groupEntity);
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public void deleteModifications(UUID groupUuid, Set<UUID> uuids) {
        List<ModificationEntity> modifications = this.modificationRepository.findAllByGroupId(groupUuid)
                .stream()
                .filter(m -> uuids.contains(m.getId()))
                .collect(Collectors.toList());
        this.modificationRepository.deleteAll(modifications);
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }
}
