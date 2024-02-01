/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import lombok.NonNull;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
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

    private final EquipmentModificationRepositories equipmentModificationRepositories;

    private static final String MODIFICATION_NOT_FOUND_MESSAGE = "Modification (%s) not found";

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository, ModificationRepository modificationRepository, EquipmentModificationRepositories equipmentModificationRepositories) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
        this.equipmentModificationRepositories = equipmentModificationRepositories;
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
        equipmentModificationRepositories.deleteAll();
    }

    @Transactional // To have all create in the same transaction (atomic)
    // TODO Remove transaction when errors will no longer be sent to the front
    public void saveModifications(UUID groupUuid, List<? extends ModificationEntity> modifications) {
        var modificationGroupEntity = this.modificationGroupRepository
                .findById(groupUuid)
                .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        modifications.forEach(m -> {
            modificationGroupEntity.addModification(m);
            // We need here to call the save() method on the modification entity cause the ids of the ModificationEntity's are used in further treatments in the same transaction.
            // As we generate the id in Java with @GeneratedValue(strategy = GenerationType.AUTO), saving the entity in the JPA world is enough to generate the id (no need to flush it).
            // Without the saving, the id generation would be done only at the flush() and wouldn't be available for the further treatments.
            modificationRepository.save(m);
        });
    }

    @Transactional
    // TODO Remove transaction when errors will no longer be sent to the front
    public List<ModificationEntity> moveModifications(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsUuid, UUID referenceModificationUuid) {
        ModificationGroupEntity originModificationGroupEntity = getModificationGroup(originGroupUuid);

        Map<UUID, ModificationEntity> originModifications = getModificationsEntities(originGroupUuid).stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

        List<UUID> modificationsToMoveUUID = modificationsUuid.stream().filter(originModifications::containsKey).collect(Collectors.toList());

        List<ModificationEntity> newDestinationModificationList = List.of();
        if (!modificationsToMoveUUID.isEmpty()) {
            // if moving within the same group
            if (originGroupUuid.equals(destinationGroupUuid)) {
                if (referenceModificationUuid != null && !originModifications.containsKey(referenceModificationUuid)) {
                    throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
                }

                newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, originModifications, referenceModificationUuid);

                originModificationGroupEntity.setModifications(newDestinationModificationList);
            } else {
                //if destination is empty, group does not exist, we create it here if needed
                ModificationGroupEntity destinationModificationGroupEntity = getOrCreateModificationGroup(destinationGroupUuid);

                Map<UUID, ModificationEntity> destinationModifications = getModificationsEntities(destinationGroupUuid).stream()
                        .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));

                // referenceModificationUuid must belong to destination one
                if (referenceModificationUuid != null && !destinationModifications.containsKey(referenceModificationUuid)) {
                    throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
                }

                newDestinationModificationList = updateModificationList(modificationsToMoveUUID, originModifications, destinationModifications, referenceModificationUuid);

                originModificationGroupEntity.setModifications(new ArrayList<>(originModifications.values()));
                destinationModificationGroupEntity.setModifications(newDestinationModificationList);
            }
        }
        return newDestinationModificationList;
    }

    private List<ModificationEntity> updateModificationList(List<UUID> modificationsToMoveUuid, Map<UUID, ModificationEntity> originModifications, Map<UUID, ModificationEntity> destinationModifications, UUID referenceModificationUuid) {
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

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound, false);
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound, boolean onlyStashed) {
        try {
            return onlyMetadata ? getModificationsMetadata(groupUuid, onlyStashed) : getModificationsInfos(List.of(groupUuid), onlyStashed);
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    public List<ModificationInfos> getModificationsMetadata(UUID groupUuid, boolean onlyStashed) {
        Stream<ModificationEntity> modificationEntitySteam = modificationRepository
                .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
                .stream();
        if (onlyStashed) {
            return modificationEntitySteam.filter(m -> m.getStashed())
                    .map(this::getModificationInfos)
                    .collect(Collectors.toList());
        } else {
            return modificationEntitySteam
                    .map(this::getModificationInfos)
                    .collect(Collectors.toList());
        }
    }

    public TabularModificationEntity loadTabularModificationSubEntities(TabularModificationEntity tabularModificationEntity) {
        switch (tabularModificationEntity.getModificationType()) {
            case GENERATOR_MODIFICATION:
                equipmentModificationRepositories.getGeneratorModificationRepository().findAllWithReactiveCapabilityCurvePointsByIdIn(tabularModificationEntity.getModifications().stream().map(ModificationEntity::getId).toList());
                break;
            default:
                break;
        }
        return tabularModificationEntity;
    }

    public ModificationInfos getModificationInfos(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof TabularModificationEntity tabularModificationEntity) {
            return loadTabularModificationSubEntities(tabularModificationEntity).toModificationInfos();
        }
        return modificationEntity.toModificationInfos();
    }

    public List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids, boolean onlyStashed) {
        Stream<ModificationEntity> modificationEntity = groupUuids.stream().flatMap(this::getModificationEntityStream);
        if (onlyStashed) {
            return modificationEntity.filter(m -> m.getStashed() == onlyStashed)
                    .map(this::getModificationInfos)
                    .collect(Collectors.toList());
        } else {
            return modificationEntity.map(this::getModificationInfos)
                .collect(Collectors.toList());
        }
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        Optional<ModificationEntity> optionalModificationEntity = modificationRepository.findById(modificationUuid);
        if (!optionalModificationEntity.isPresent()) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString());
        }
        ModificationEntity modificationEntity = optionalModificationEntity.get();
        if (modificationEntity instanceof TabularModificationEntity tabularModificationEntity) {
            List<UUID> ids = modificationRepository.findSubModificationsIds(modificationUuid);
            tabularModificationEntity.setModifications(equipmentModificationRepositories.findSubEntities(tabularModificationEntity.getModificationType(), ids));
            return getModificationInfos(tabularModificationEntity);
        }
        return getModificationInfos(modificationEntity);
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                groupEntity.getModifications().forEach(modificationEntity -> {
                    if (modificationEntity instanceof TabularModificationEntity tabularModificationEntity) {
                        equipmentModificationRepositories.deleteTabularModification(tabularModificationEntity);
                    } else {
                        modificationRepository.delete(modificationEntity);
                    }
                });
//                modificationRepository.deleteAll(groupEntity.getModifications().stream().filter(Objects::nonNull).collect(Collectors.toList()));
            }
            this.modificationGroupRepository.delete(groupEntity);
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return;
            }
            throw e;
        }
    }

    @Transactional
    public int deleteModifications(UUID groupUuid, boolean onlyStashed) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        List<ModificationEntity> modifications;
        if (onlyStashed) {
            modifications = getModificationEntityStream(groupUuid)
                .filter(ModificationEntity::getStashed)
                .collect(Collectors.toList());
        } else {
            modifications = getModificationEntityStream(groupUuid).collect(Collectors.toList());
        }
        modifications.forEach(groupEntity::removeModification);
        int count = modifications.size();
        this.modificationRepository.deleteAll(modifications);
        return count;
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public int deleteModifications(UUID groupUuid, List<UUID> uuids) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        List<ModificationEntity> modifications = getModificationEntityStream(groupUuid)
                .filter(m -> uuids.contains(m.getId()))
                .collect(Collectors.toList());
        modifications.forEach(groupEntity::removeModification);
        int count = modifications.size();
        this.modificationRepository.deleteAll(modifications);
        return count;
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationException(MODIFICATION_GROUP_NOT_FOUND, groupUuid.toString()));
    }

    private ModificationGroupEntity getOrCreateModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
    }

    private Stream<ModificationEntity> getModificationEntityStream(UUID groupUuid) {
        return getModificationGroup(groupUuid).getModifications().stream().filter(Objects::nonNull);
    }

    private List<ModificationEntity> getModificationsEntities(UUID groupUuid) {
        return getModificationEntityStream(groupUuid).collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public Integer getModificationsCount(@NonNull UUID groupUuid, boolean stashed) {
        return modificationRepository.countByGroupIdAndStashed(groupUuid, stashed);
    }

    @Transactional(readOnly = true)
    public List<ModificationEntity> getModificationsEntities(@NonNull List<UUID> uuids) {
        // Spring-data findAllById doc says: the order of elements in the result is not guaranteed
        List<ModificationEntity> entities = modificationRepository.findAllById(uuids);
        entities.sort(Comparator.comparing(e -> uuids.indexOf(e.getId())));
        return entities;
    }

    @Transactional(readOnly = true)
    public List<ModificationEntity> copyModificationsEntities(@NonNull UUID groupUuid) {
        return getModificationEntityStream(groupUuid).filter(m -> !m.getStashed()).map(ModificationEntity::copy).collect(Collectors.toList());
    }

    @Transactional
    public void stashNetworkModifications(@NonNull List<UUID> modificationUuids) {
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                    .findById(modificationUuid)
                    .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            modificationEntity.setStashed(true);
            this.modificationRepository.save(modificationEntity);
        }
    }

    @Transactional
    public void restoreNetworkModifications(@NonNull List<UUID> modificationUuids) {
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                    .findById(modificationUuid)
                    .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            modificationEntity.setStashed(false);
            this.modificationRepository.save(modificationEntity);
        }
    }

    @Transactional
    public void updateModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        this.modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)))
            .update(modificationInfos);
    }

    @Transactional
    public void deleteStashedModificationInGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            List<UUID> stashedModificationUuids = groupEntity.getModifications().stream()
                    .filter(modification -> modification != null && modification.getStashed())
                    .map(ModificationEntity::getId).collect(Collectors.toList());
            if (!stashedModificationUuids.isEmpty()) {
                deleteModifications(groupUuid, stashedModificationUuids);
            }
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return;
            }
            throw e;
        }
    }
}
