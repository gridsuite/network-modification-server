/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.entities.AbstractModificationEntity;
import org.gridsuite.modification.server.entities.ElementaryModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

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

    public ElementaryModificationEntity insertElementaryModification(UUID groupId, ElementaryModificationEntity elementaryModificationEntity) {
        ModificationGroupEntity modificationGroupEntity = modificationGroupRepository.findById(groupId).orElse(new ModificationGroupEntity(groupId));
        this.modificationRepository.save(elementaryModificationEntity);
        modificationGroupEntity.getModifications().add(elementaryModificationEntity);
        this.modificationGroupRepository.save(modificationGroupEntity);
        return elementaryModificationEntity;
    }

    public List<ModificationGroupEntity> getModificationGroups() {
        return this.modificationGroupRepository.findAll();
    }

    public Optional<ModificationGroupEntity> getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid);
    }

    public List<ElementaryModificationEntity> getElementaryModifications() {
        return this.modificationRepository.getElementaryModifications();
    }

    public List<ElementaryModificationEntity> getElementaryModifications(UUID groupUuid) {
        return (this.modificationGroupRepository.findById(groupUuid).orElseThrow().getModifications().stream()
                .filter(m -> m.getType().equals(ModificationType.ELEMENTARY.name())))
                .map(ElementaryModificationEntity.class::cast)
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

    public List<AbstractModificationEntity> getModifications(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow().getModifications();
    }
}
