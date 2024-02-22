/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import lombok.NonNull;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.dto.TabularModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.ModificationGroupEntity;
import org.gridsuite.modification.server.entities.TabularCreationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
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

    private final GeneratorModificationRepository generatorModificationRepository;

    private static final String MODIFICATION_NOT_FOUND_MESSAGE = "Modification (%s) not found";

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository, ModificationRepository modificationRepository, GeneratorModificationRepository generatorModificationRepository) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
        this.generatorModificationRepository = generatorModificationRepository;
    }

    public enum StashOption {
        STASHED,
        UNSTASHED,
        ANY
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteAll() {
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
    }

    @Transactional // To have all create in the same transaction (atomic)
    // TODO Remove transaction when errors will no longer be sent to the front
    // This method should be package-private and not used as API of the service as it uses ModificationEntity and
    // we want to encapsulate the use of Entity related objects to this service.
    // Nevertheless We have to keep it public for transactional annotation.
    public void saveModifications(UUID groupUuid, List<? extends ModificationEntity> modifications) {
        saveModificationsNonTransactional(groupUuid, modifications);
    }

    @Transactional // To have all create in the same transaction (atomic)
    // TODO Remove transaction when errors will no longer be sent to the front
    public void saveModificationInfos(UUID groupUuid, List<? extends ModificationInfos> modifications) {
        saveModificationsNonTransactional(groupUuid, modifications.stream().map(ModificationInfos::toEntity).toList());
    }

    private void saveModificationsNonTransactional(UUID groupUuid, List<? extends ModificationEntity> modifications) {
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

        if (originGroupUuid.equals(destinationGroupUuid)) {
            List<ModificationEntity> modificationEntities = getModificationsMetadataEntities(originGroupUuid, StashOption.UNSTASHED);
            Map<UUID, ModificationEntity> originModifications = modificationEntities.stream()
                    .collect(Collectors.toMap(ModificationEntity::getId, Function.identity(), (x, y) -> y, LinkedHashMap::new));
            List<UUID> modificationsToMoveUUID = modificationsUuid.stream().filter(originModifications::containsKey).collect(Collectors.toList());
            if (referenceModificationUuid != null && !originModifications.containsKey(referenceModificationUuid)) {
                throw new NetworkModificationException(MOVE_MODIFICATION_ERROR);
            }

            List<ModificationEntity> newList = updateModificationList(modificationsToMoveUUID, originModifications, originModifications, referenceModificationUuid);
            AtomicInteger index = new AtomicInteger(0);
            newList.forEach(m -> this.modificationRepository.setOrderById(index.getAndIncrement(), m.getId()));
            return List.of();
        }

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

    @Transactional
    public Map<UUID, UUID> duplicateModifications(List<UUID> sourceModificationUuids) {
        List<ModificationEntity> sourceEntities = modificationRepository.findAllById(sourceModificationUuids);
        // findAllById does not keep sourceModificationUuids order, but
        // sourceEntities, copyEntities, newEntities have the same order.
        List<ModificationEntity> copyEntities = sourceEntities.stream()
                .map(this::getModificationInfos)
                .map(ModificationInfos::toEntity)
                .toList();
        List<ModificationEntity> newEntities = modificationRepository.saveAll(copyEntities);

        // Iterate through sourceEntities and newEntities collections simultaneously to map sourceId -> newId
        Map<UUID, UUID> ids = new HashMap<>();
        Iterator<ModificationEntity> sourceIterator = sourceEntities.iterator();
        Iterator<ModificationEntity> newIterator = newEntities.iterator();
        while (sourceIterator.hasNext() && newIterator.hasNext()) {
            ids.put(sourceIterator.next().getId(), newIterator.next().getId());
        }
        return ids;
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

    public List<ModificationEntity> getModificationsMetadataEntities(UUID groupUuid, StashOption stashOption) {
        Stream<ModificationEntity> modificationEntityStream = modificationRepository
                .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
                .stream();
        if (stashOption == StashOption.ANY) {
            return modificationEntityStream.collect(Collectors.toList());
        } else {
            boolean stashValue = stashOption == StashOption.STASHED;
            return modificationEntityStream.filter(m -> m.getStashed() == stashValue)
                    .collect(Collectors.toList());
        }
    }

    public List<ModificationInfos> getModificationsMetadata(UUID groupUuid, boolean onlyStashed) {
        StashOption stashOption = onlyStashed ? StashOption.STASHED : StashOption.ANY;
        return getModificationsMetadataEntities(groupUuid, stashOption).stream()
                .map(this::getModificationInfos)
                .collect(Collectors.toList());
    }

    public TabularModificationInfos loadTabularModificationSubEntities(ModificationEntity modificationEntity) {
        TabularModificationEntity tabularModificationEntity = (TabularModificationEntity) modificationEntity;
        switch (tabularModificationEntity.getModificationType()) {
            case GENERATOR_MODIFICATION:
                List<UUID> subModificationsUuids = modificationRepository.findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(modificationEntity.getId());
                // We retrieve generator modifications by generatorModificationRepository and store them as a map by IDs to re-order them later on
                Map<UUID, GeneratorModificationEntity> generatorModifications = generatorModificationRepository
                    .findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids)
                    .stream()
                    .collect(Collectors.toMap(
                        ModificationEntity::getId,
                        Function.identity()
                    ));
                // We load properties on the generators, it uses hibernate first-level cache to fill them up directly in the map
                generatorModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
                // Then we can re-order the list of GeneratorModificationEntity based on ordered list of IDs
                List<GeneratorModificationEntity> orderedGeneratorModifications = subModificationsUuids
                    .stream()
                    .map(generatorModifications::get)
                    .toList();
                return TabularModificationInfos.builder()
                    .uuid(tabularModificationEntity.getId())
                    .date(tabularModificationEntity.getDate())
                    .stashed(tabularModificationEntity.getStashed())
                    .modificationType(tabularModificationEntity.getModificationType())
                    .modifications(orderedGeneratorModifications.stream().map(GeneratorModificationEntity::toModificationInfos).map(m -> (ModificationInfos) m).toList())
                    .build();
            default:
                break;
        }
        return tabularModificationEntity.toModificationInfos();
    }

    public TabularCreationEntity loadTabularCreationSubEntities(ModificationEntity modificationEntity) {
        TabularCreationEntity tabularCreationEntity = (TabularCreationEntity) modificationEntity;
        switch (tabularCreationEntity.getCreationType()) {
            case GENERATOR_CREATION:
                tabularCreationEntity = modificationRepository.findTabularCreationWithReactiveCapabilityCurvePointsById(modificationEntity.getId()).orElseThrow(() ->
                    new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationEntity.getId()))
                );
                modificationRepository.findAllCreationsWithReactiveCapabilityCurvePointsByIdIn(tabularCreationEntity.getCreations().stream().map(ModificationEntity::getId).toList());
                break;
            default:
                break;
        }
        return tabularCreationEntity;
    }

    public ModificationInfos getModificationInfos(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof TabularModificationEntity) {
            return loadTabularModificationSubEntities(modificationEntity);
        } else if (modificationEntity instanceof TabularCreationEntity) {
            return loadTabularCreationSubEntities(modificationEntity).toModificationInfos();
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
        return getModificationInfos(optionalModificationEntity.get());
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
        List<ModificationEntity> modifications;
        if (groupUuid != null) {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            modifications = getModificationEntityStream(groupUuid)
                    .filter(m -> uuids.contains(m.getId()))
                    .collect(Collectors.toList());
            modifications.forEach(groupEntity::removeModification);
        } else {
            modifications = modificationRepository.findAllById(uuids);
            Optional<ModificationEntity> optionalModificationWithGroup = modifications.stream().filter(m -> m.getGroup() != null).findFirst();
            if (optionalModificationWithGroup.isPresent()) {
                throw new NetworkModificationException(MODIFICATION_DELETION_ERROR, String.format("%s is owned by group %s",
                    optionalModificationWithGroup.get().getId().toString(), optionalModificationWithGroup.get().getGroup().getId()));
            }
        }
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
    public List<ModificationInfos> getModificationsInfos(@NonNull List<UUID> uuids) {
        // Spring-data findAllById doc says: the order of elements in the result is not guaranteed
        Map<UUID, ModificationEntity> entities = modificationRepository.findAllById(uuids)
            .stream()
            .collect(Collectors.toMap(
                ModificationEntity::getId,
                Function.identity()
            ));
        return uuids.stream().map(entities::get).filter(Objects::nonNull).map(this::getModificationInfos).toList();
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getActiveModificationsInfos(@NonNull UUID groupUuid) {
        return getModificationEntityStream(groupUuid).filter(m -> !m.getStashed()).map(this::getModificationInfos).toList();
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

    @Transactional(readOnly = true)
    public List<ModificationMetadata> getModificationsMetadata(List<UUID> uuids) {
        // custom query to read only the required fields (id/type)
        return modificationRepository.findMetadataIn(uuids)
            .stream()
            .map(entity -> ModificationMetadata.builder()
                    .id(entity.getId())
                    .type(ModificationType.valueOf(entity.getType()))
                    .build())
            .toList();
    }
}
