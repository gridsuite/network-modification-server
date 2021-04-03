/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ElementaryModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.AbstractModificationEntity;
import org.gridsuite.modification.server.entities.ElementaryModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
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

    public void deleteAll() {
        modificationGroupRepository.deleteAll();
        modificationRepository.deleteAll();
    }

    @Transactional
    public ElementaryModificationEntity insertElementaryModification(UUID groupId, ElementaryModificationEntity elementaryModificationEntity) {
        ModificationGroupEntity modificationGroupEntity = modificationGroupRepository.findById(groupId).orElse(new ModificationGroupEntity(groupId));
        this.modificationRepository.save(elementaryModificationEntity);
        modificationGroupEntity.getModifications().add(elementaryModificationEntity);
        this.modificationGroupRepository.save(modificationGroupEntity);
        return elementaryModificationEntity;
    }

    // No transactional because (no prefetch for lazy loading)
    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getUuid)
                .collect(Collectors.toList());
    }

    public Optional<ModificationGroupEntity> getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid);
    }

    public List<ElementaryModificationEntity> getElementaryModifications() {
        return this.modificationRepository.getElementaryModifications();
    }

    @Transactional(readOnly = true) // because (prefetch for lazy loading)
    public List<ElementaryModificationInfos> getElementaryModifications(UUID groupUuid) {
        return (this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()))
                .getModifications()
                .stream()
                .filter(m -> m.getType().equals(ModificationType.ELEMENTARY.name())))
                .map(ElementaryModificationEntity.class::cast)
                .map(ElementaryModificationEntity::toElementaryModificationInfos)
                .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()))
                .getModifications()
                .stream()
                .map(AbstractModificationEntity::toModificationInfos)
                .collect(Collectors.toList());
    }

    public void deleteModificationGroup(UUID groupUuid) {
        this.modificationGroupRepository.deleteById(groupUuid);
    }

    @Transactional
    public void deleteModifications(UUID groupUuid, Set<UUID> uuids) {
        ModificationGroupEntity group = this.modificationGroupRepository.findById(groupUuid).orElseThrow();
        List<AbstractModificationEntity> modifications = group.getModifications().stream().filter(m -> uuids.contains(m.getUuid())).collect(Collectors.toList());
        group.getModifications().removeAll(modifications);
    }
}
