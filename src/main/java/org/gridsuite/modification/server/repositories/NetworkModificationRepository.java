/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NonNull;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BranchStatusModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
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

    @Transactional // To have all create in the same transaction (atomic)
    public void saveModifications(UUID groupUuid, List<? extends ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(modificationGroupEntity::addModification);
    }

    @Getter
    @AllArgsConstructor
    public class MoveModificationResult {
        private List<UUID> modificationsMoved;
        private List<UUID> modificationsInError;
    }

    @Transactional // To have all move in the same transaction (atomic)
    //when we move modifications, we move them right before referenceModification when it is defined, at the end of list otherwise
    public MoveModificationResult moveModifications(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsUuid, UUID referenceModificationUuid) {
        ModificationGroupEntity originModificationGroupEntity = getModificationGroup(originGroupUuid);

        Map<UUID, ModificationEntity> originModifications = modificationRepository.findAllBaseByGroupId(originGroupUuid).stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

        List<UUID> modificationsInErrorUUID = modificationsUuid.stream().filter(mUuid -> !originModifications.containsKey(mUuid)).collect(Collectors.toList());
        List<UUID> modificationsToMoveUUID = modificationsUuid.stream().filter(originModifications::containsKey).collect(Collectors.toList());

        if (!modificationsToMoveUUID.isEmpty()) {

            // if moving within the same group
            if (originGroupUuid.equals(destinationGroupUuid)) {
                if (referenceModificationUuid != null && !originModifications.containsKey(referenceModificationUuid)) {
                    throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
                }

                List<ModificationEntity> newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, originModifications, referenceModificationUuid);

                originModificationGroupEntity.setModifications(newDestinationModificationList);
            } else {
                //if destination is empty, group does not exist, we create it here if needed
                ModificationGroupEntity destinationModificationGroupEntity = getOrCreateModificationGroup(destinationGroupUuid);

                Map<UUID, ModificationEntity> destinationModifications = modificationRepository.findAllBaseByGroupId(destinationGroupUuid).stream()
                        .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

                // referenceModificationUuid must belong to destination one
                if (referenceModificationUuid != null && !destinationModifications.containsKey(referenceModificationUuid)) {
                    throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
                }

                List<ModificationEntity> newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, destinationModifications, referenceModificationUuid);

                originModificationGroupEntity.setModifications(new ArrayList<>(originModifications.values()));
                destinationModificationGroupEntity.setModifications(newDestinationModificationList);
            }
        }
        return new MoveModificationResult(modificationsToMoveUUID, modificationsInErrorUUID);
    }

    public List<ModificationEntity> updateModificationList(List<UUID> modificationsToMoveUuid, Map<UUID, ModificationEntity> originModifications, Map<UUID, ModificationEntity> destinationModifications, UUID referenceModificationUuid) {
        List<ModificationEntity> movedModifications = modificationsToMoveUuid.stream().map(originModifications::remove).collect(Collectors.toList());

        List<ModificationEntity> newDestinationModificationList = new ArrayList<>(destinationModifications.values());
        /* when referenceModification == null we move at the end of list */
        int index = referenceModificationUuid == null ? newDestinationModificationList.size() : newDestinationModificationList.indexOf(destinationModifications.get(referenceModificationUuid));
        newDestinationModificationList.addAll(index, movedModifications);

        return newDestinationModificationList;
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModifications(List<UUID> uuids) {
        return this.modificationRepository.findAllById(uuids).stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        try {
            return onlyMetadata ? getModificationsMetadata(groupUuid) : getModificationsInfos(List.of(groupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    private List<ModificationInfos> getModificationsMetadata(UUID groupUuid) {
        return modificationRepository
            .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
            .stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    public List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids) {
        return this.getModificationsEntities(groupUuids).stream().map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString()))
            .toModificationInfos();
    }

    public Stream<ModificationEntity> getModificationEntityList(UUID groupUuid) {
        return getModificationGroup(groupUuid).getModifications().stream().filter(Objects::nonNull);
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                modificationRepository.deleteAll(groupEntity.getModifications().stream().filter(Objects::nonNull).collect(Collectors.toList()));
            }
            this.modificationGroupRepository.delete(groupEntity);
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return;
            }
            throw e;
        }
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public int deleteModifications(UUID groupUuid, List<UUID> uuids) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        List<ModificationEntity> modifications = getModificationEntityList(groupUuid)
                .filter(m -> uuids.contains(m.getId()))
                .collect(Collectors.toList());
        modifications.forEach(groupEntity::removeModification);
        int count = modifications.size();
        this.modificationRepository.deleteAll(modifications);
        return count;
    }

    public void updateModification(ModificationEntity modificationEntity) {
        this.modificationRepository.save(modificationEntity);
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }

    private ModificationGroupEntity getOrCreateModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
    }

    private List<ModificationEntity> getModificationsEntities(List<UUID> groupUuids) {
        return groupUuids.stream().flatMap(this::getModificationEntityList).collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public Optional<ModificationEntity> cloneModificationEntity(UUID modificationUuid) {
        Optional<ModificationEntity> entity = modificationRepository.findById(modificationUuid);
        entity.ifPresent(ModificationEntity::cloneWithIdsToNull);
        return entity;
    }

    @Transactional(readOnly = true)
    public List<ModificationEntity> cloneModificationsEntities(@NonNull UUID groupUuid) {
        return getModificationEntityList(groupUuid).map(entity -> {
            entity.cloneWithIdsToNull();
            return entity;
        }).collect(Collectors.toList());
    }
}
