/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import lombok.NonNull;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.*;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Repository
public class NetworkModificationRepository {
    private final ModificationGroupRepository modificationGroupRepository;

    private final ModificationRepository modificationRepository;

    private final GeneratorModificationRepository generatorModificationRepository;

    private final GeneratorCreationRepository generatorCreationRepository;

    private final ModificationApplicationInfosService modificationApplicationInfosService;

    private static final String MODIFICATION_NOT_FOUND_MESSAGE = "Modification (%s) not found";

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository,
                                         ModificationRepository modificationRepository,
                                         GeneratorModificationRepository generatorModificationRepository,
                                         GeneratorCreationRepository generatorCreationRepository,
                                         ModificationApplicationInfosService modificationApplicationInfosService) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
        this.generatorModificationRepository = generatorModificationRepository;
        this.generatorCreationRepository = generatorCreationRepository;
        this.modificationApplicationInfosService = modificationApplicationInfosService;
    }

    @Transactional // To have all the delete in the same transaction (atomic)
    public void deleteAll() {
        modificationApplicationInfosService.deleteAll();
        modificationRepository.deleteAll();
        modificationGroupRepository.deleteAll();
    }

    @Transactional // To have all create in the same transaction (atomic)
    // TODO Remove transaction when errors will no longer be sent to the front
    // This method should be package-private and not used as API of the service as it uses ModificationEntity and
    // we want to encapsulate the use of Entity related objects to this service.
    // Nevertheless We have to keep it public for transactional annotation.
    public List<ModificationEntity> saveModifications(UUID groupUuid, List<ModificationEntity> modifications) {
        return saveModificationsNonTransactional(groupUuid, modifications);
    }

    @Transactional // To have all create in the same transaction (atomic)
    // TODO Remove transaction when errors will no longer be sent to the front
    public List<ModificationEntity> saveModificationInfos(UUID groupUuid, List<ModificationInfos> modifications) {
        List<ModificationEntity> entities = modifications.stream().map(ModificationEntity::fromDTO).toList();

        return saveModificationsNonTransactional(groupUuid, entities);
    }

    public UUID createNetworkCompositeModification(@NonNull List<UUID> modificationUuids) {
        CompositeModificationInfos compositeInfos = CompositeModificationInfos.builder().modifications(List.of()).build();
        CompositeModificationEntity compositeEntity = (CompositeModificationEntity) ModificationEntity.fromDTO(compositeInfos);
        List<ModificationEntity> copyEntities = modificationRepository.findAllByIdIn(modificationUuids).stream()
                .map(this::getModificationInfos)
                .map(ModificationEntity::fromDTO)
                .toList();
        compositeEntity.setModifications(copyEntities);
        return modificationRepository.save(compositeEntity).getId();
    }

    public void updateCompositeModification(@NonNull UUID compositeUuid, @NonNull List<UUID> modificationUuids) {
        ModificationEntity modificationEntity = modificationRepository.findById(compositeUuid)
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, compositeUuid)));

        if (!(modificationEntity instanceof CompositeModificationEntity compositeEntity)) {
            throw new NetworkModificationException(MODIFICATION_ERROR,
                    String.format("Modification (%s) is not a composite modification", compositeUuid));
        }

        List<ModificationEntity> copyEntities = modificationRepository.findAllByIdIn(modificationUuids).stream()
                .map(this::getModificationInfos)
                .map(ModificationEntity::fromDTO)
                .toList();
        compositeEntity.getModifications().clear();
        compositeEntity.getModifications().addAll(copyEntities);
        modificationRepository.save(compositeEntity);
    }

    private List<ModificationEntity> saveModificationsNonTransactional(UUID groupUuid, List<ModificationEntity> modifications) {
        int order = modificationRepository.countByGroupIdAndStashed(groupUuid, false);
        var modificationGroupEntity = this.modificationGroupRepository
            .findById(groupUuid)
            .orElseGet(() -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
        for (ModificationEntity m : modifications) {
            modificationGroupEntity.addModification(m);
            m.setModificationsOrder(order);
            order++;
        }
        return modificationRepository.saveAll(modifications);
    }

    @Transactional
    // TODO Remove transaction when errors will no longer be sent to the front
    public List<ModificationEntity> moveModifications(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsToMoveUUID, UUID referenceModificationUuid) {
        // read origin group and modifications
        ModificationGroupEntity originModificationGroupEntity = getModificationGroup(originGroupUuid);
        List<ModificationEntity> originModificationEntities = originModificationGroupEntity.getModifications()
            .stream()
            .filter(modificationEntity -> !modificationEntity.getStashed())
            .collect(Collectors.toList());
        // To remove null entities when @orderColumn is not a contiguous sequence starting from 0 (to be fixed?)
        // (there are several places in this file where we filter non-null modification entities)
        originModificationEntities.removeIf(Objects::isNull);

        // remove from origin list
        List<ModificationEntity> modificationsToMove = removeModifications(originModificationEntities, modificationsToMoveUUID);
        if (modificationsToMove.isEmpty()) {
            return List.of();
        }

        if (originGroupUuid.equals(destinationGroupUuid)) { // single group case
            // insert into origin list
            insertModifications(originModificationEntities, modificationsToMove, referenceModificationUuid);
        } else { // 2-group case
            // before moving origin modifications between nodes, remove applications since they are not applicable anymore
            modificationApplicationInfosService.deleteAllByModificationIds(modificationsToMove.stream().map(ModificationEntity::getId).collect(Collectors.toList()));
            // read destination group and modifications (group must be created if missing)
            ModificationGroupEntity destinationModificationGroupEntity = getOrCreateModificationGroup(destinationGroupUuid);
            List<ModificationEntity> destinationModificationEntities = destinationModificationGroupEntity.getModifications();
            destinationModificationEntities.removeIf(Objects::isNull);
            // insert into destination list
            insertModifications(destinationModificationEntities, modificationsToMove, referenceModificationUuid);
            // update destination group
            destinationModificationGroupEntity.setModifications(destinationModificationEntities);
        }

        // update origin group
        originModificationGroupEntity.setModifications(originModificationEntities);

        return modificationsToMove;
    }

    private void insertModifications(List<ModificationEntity> modificationsList, List<ModificationEntity> modificationsToAdd, UUID referenceModificationUuid) {
        // If referenceModificationUuid == null we will append at the end of list, otherwise we will insert before referenceModification
        int insertionIndex = referenceModificationUuid == null ?
                modificationsList.size() : IntStream.range(0, modificationsList.size())
                        .filter(i -> referenceModificationUuid.equals(modificationsList.get(i).getId()))
                        .findFirst()
                        .orElseThrow(() -> new NetworkModificationException(MOVE_MODIFICATION_ERROR));
        modificationsList.addAll(insertionIndex, modificationsToAdd);
        for (int order = 0; order < modificationsList.size(); order++) {
            modificationsList.get(order).setModificationsOrder(order);
        }
    }

    private List<ModificationEntity> removeModifications(List<ModificationEntity> modificationsList, List<UUID> orderedIdsToRemove) {
        // Remove all UUID from modificationsList, in a single loop starting from the end.
        // We memorize the removed elements in a map, to return them in the same order as in orderedIdsToRemove.
        Set<UUID> uniqueUuids = new HashSet<>(orderedIdsToRemove);
        Map<UUID, ModificationEntity> removedModificationsMap = new HashMap<>();
        IntStream.iterate(modificationsList.size() - 1, i -> i >= 0, i -> i - 1).forEach(i -> {
            UUID modificationId = modificationsList.get(i).getId();
            if (uniqueUuids.contains(modificationId)) {
                removedModificationsMap.put(modificationId, modificationsList.remove(i));
            }
        });
        List<ModificationEntity> removedModifications = new ArrayList<>();
        orderedIdsToRemove.forEach(i -> {
            ModificationEntity e = removedModificationsMap.get(i);
            if (e != null) {
                removedModifications.add(e);
            }
        });
        return removedModifications;
    }

    public List<UUID> getModificationGroupsUuids() {
        return this.modificationGroupRepository.findAll().stream()
                .map(ModificationGroupEntity::getId)
                .collect(Collectors.toList());
    }

    @Transactional
    public Map<UUID, UUID> duplicateCompositeModifications(List<UUID> sourceModificationUuids) {
        List<ModificationEntity> sourceEntities = modificationRepository.findAllById(sourceModificationUuids);
        // findAllById does not keep sourceModificationUuids order, but
        // sourceEntities, copyEntities, newEntities have the same order.
        List<ModificationEntity> copyEntities = sourceEntities.stream()
                .map(this::getModificationInfos)
                .map(ModificationEntity::fromDTO)
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

    public List<ModificationInfos> getModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound, boolean onlyStashed) {
        try {
            return onlyMetadata ? getModificationsMetadata(groupUuid, onlyStashed) : getModificationsEntities(List.of(groupUuid), onlyStashed).stream().map(this::getModificationInfos).toList();
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    public List<ModificationInfos> getModificationsMetadata(UUID groupUuid, boolean onlyStashed) {
        if (onlyStashed) {
            return modificationRepository
                .findAllBaseByGroupIdReverse(getModificationGroup(groupUuid).getId())
                .stream()
                .filter(ModificationEntity::getStashed)
                .map(this::getModificationInfos)
                .collect(Collectors.toList());
        } else {
            return modificationRepository
                .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
                .stream()
                .map(this::getModificationInfos)
                .collect(Collectors.toList());
        }
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
                    .activated(tabularModificationEntity.getActivated())
                    .modificationType(tabularModificationEntity.getModificationType())
                    .modifications(orderedGeneratorModifications.stream().map(GeneratorModificationEntity::toModificationInfos).map(m -> (ModificationInfos) m).toList())
                    .build();
            default:
                break;
        }
        return tabularModificationEntity.toModificationInfos();
    }

    public TabularCreationInfos loadTabularCreationSubEntities(ModificationEntity modificationEntity) {
        TabularCreationEntity tabularCreationEntity = (TabularCreationEntity) modificationEntity;
        switch (tabularCreationEntity.getCreationType()) {
            case GENERATOR_CREATION:
                List<UUID> subModificationsUuids = modificationRepository.findSubModificationIdsByTabularCreationIdOrderByModificationsOrder(modificationEntity.getId());
                Map<UUID, GeneratorCreationEntity> generatorCreations = generatorCreationRepository.findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids)
                        .stream()
                        .collect(Collectors.toMap(
                                ModificationEntity::getId,
                                Function.identity()
                        ));
                generatorCreationRepository.findAllPropertiesByIdIn(subModificationsUuids);
                List<GeneratorCreationEntity> orderedGeneratorCreation = subModificationsUuids
                        .stream()
                        .map(generatorCreations::get)
                        .toList();
                return TabularCreationInfos.builder()
                        .uuid(tabularCreationEntity.getId())
                        .date(tabularCreationEntity.getDate())
                        .stashed(tabularCreationEntity.getStashed())
                        .activated(tabularCreationEntity.getActivated())
                        .creationType(tabularCreationEntity.getCreationType())
                        .creations(orderedGeneratorCreation.stream().map(GeneratorCreationEntity::toModificationInfos).map(m -> (ModificationInfos) m).toList())
                        .build();
            default:
                break;
        }
        return tabularCreationEntity.toModificationInfos();
    }

    public ModificationInfos getModificationInfos(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof TabularModificationEntity) {
            return loadTabularModificationSubEntities(modificationEntity);
        } else if (modificationEntity instanceof TabularCreationEntity) {
            return loadTabularCreationSubEntities(modificationEntity);
        }
        return modificationEntity.toModificationInfos();
    }

    public List<ModificationEntity> getModificationsEntities(List<UUID> groupUuids, boolean onlyStashed) {
        Stream<ModificationEntity> entityStream = groupUuids.stream().flatMap(this::getModificationEntityStream);
        if (onlyStashed) {
            return entityStream.filter(m -> m.getStashed() == onlyStashed).toList();
        } else {
            return entityStream.toList();
        }
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return getModificationInfos(getModificationEntity(modificationUuid));
    }

    public ModificationEntity getModificationEntity(UUID modificationUuid) {
        return modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString()));
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                deleteModifications(groupEntity.getModifications().stream().filter(Objects::nonNull).toList());
                groupEntity.getModifications().clear();
            }
            modificationGroupRepository.delete(groupEntity);
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return;
            }
            throw e;
        }
    }

    @Transactional // To have the find and delete in the same transaction (atomic)
    public int deleteModifications(UUID groupUuid, List<UUID> uuids) {
        List<ModificationEntity> modifications;
        if (groupUuid != null) {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            Stream<ModificationEntity> modificationStream = getModificationEntityStream(groupUuid);
            if (uuids != null) {
                modificationStream = modificationStream.filter(m -> uuids.contains(m.getId()));
            }
            modifications = modificationStream.collect(Collectors.toList());
            groupEntity.getModifications().removeAll(modifications); // No need to remove the group from the modification as we're going to delete it
        } else if (uuids != null) {
            modifications = modificationRepository.findAllById(uuids);
            Optional<ModificationEntity> optionalModificationWithGroup = modifications.stream().filter(m -> m.getGroup() != null).findFirst();
            if (optionalModificationWithGroup.isPresent()) {
                throw new NetworkModificationException(MODIFICATION_DELETION_ERROR, String.format("%s is owned by group %s",
                    optionalModificationWithGroup.get().getId().toString(), optionalModificationWithGroup.get().getGroup().getId()));
            }
        } else {
            throw new NetworkModificationException(MODIFICATION_DELETION_ERROR, "need to specify the group or give a list of UUIDs");
        }
        int count = modifications.size();
        deleteModifications(modifications);
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

    /**
     * returns the data from all the network modifications contained in the composite modification sent as parameter
     * but only returns the basic data common to all the modifications form the ModificationInfos, not from the extended classes
     */
    @Transactional(readOnly = true)
    public List<ModificationInfos> getBasicNetworkModificationsFromComposite(@NonNull UUID uuid) {
        List<UUID> networkModificationsUuids = modificationRepository.findModificationIdsByCompositeModificationId(uuid);
        Map<UUID, ModificationEntity> entitiesById = modificationRepository.findBaseDataByIdIn(networkModificationsUuids).stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity()));
        return networkModificationsUuids.stream()
                .map(entitiesById::get)
                .filter(Objects::nonNull)
                .map(this::getModificationInfos)
                .toList();
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getCompositeModificationsInfos(@NonNull List<UUID> uuids) {
        List<ModificationInfos> entities = new ArrayList<>();
        uuids.forEach(uuid -> {
            List<UUID> foundEntities = modificationRepository.findModificationIdsByCompositeModificationId(uuid);
            List<ModificationInfos> orderedModifications = foundEntities
                    .stream()
                    .map(this::getModificationInfo)
                    .toList();
            entities.addAll(orderedModifications);
        }
        );
        return entities;
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getActiveModificationsInfos(@NonNull UUID groupUuid) {
        return getModificationEntityStream(groupUuid).filter(m -> !m.getStashed()).map(this::getModificationInfos).toList();
    }

    @Transactional
    public void stashNetworkModifications(@NonNull List<UUID> modificationUuids, int stashedModificationCount) {
        int stashModificationOrder = -stashedModificationCount - 1;
        List<ModificationEntity> modificationEntities = new ArrayList<>();
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                    .findById(modificationUuid)
                    .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            modificationEntity.setStashed(true);
            modificationEntity.setModificationsOrder(stashModificationOrder);
            modificationEntities.add(modificationEntity);
            stashModificationOrder--;
        }
        this.modificationRepository.saveAll(modificationEntities);
    }

    @Transactional
    public void reorderNetworkModifications(UUID groupId, Boolean stashed) {
        List<ModificationEntity> entities = this.modificationRepository.findAllByGroupId(groupId, stashed);
        if (!entities.isEmpty()) {
            if (Boolean.TRUE.equals(stashed)) {
                IntStream.range(1, entities.size() + 1)
                    .forEach(i -> entities.get(i - 1).setModificationsOrder(-i));
            } else {
                IntStream.range(0, entities.size())
                    .forEach(i -> entities.get(i).setModificationsOrder(i));
            }
        }
        this.modificationRepository.saveAll(entities);
    }

    @Transactional
    public void restoreNetworkModifications(@NonNull List<UUID> modificationUuids, int unstashedSize) {
        int modificationOrder = unstashedSize;
        List<ModificationEntity> modifications = modificationRepository.findAllByIdInReverse(modificationUuids);
        if (modifications.size() != modificationUuids.size()) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND);
        }
        for (ModificationEntity modification : modifications) {
            modification.setStashed(false);
            modification.setModificationsOrder(modificationOrder++);
        }
        this.modificationRepository.saveAll(modifications);
    }

    @Transactional
    public void updateNetworkModificationsActivation(@NonNull List<UUID> modificationUuids, boolean activated) {
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                .findById(modificationUuid)
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            modificationEntity.setActivated(activated);
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
            List<ModificationEntity> modifications = getModificationEntityStream(groupUuid)
                .filter(Objects::nonNull)
                .filter(ModificationEntity::getStashed)
                .toList();
            if (!modifications.isEmpty()) {
                groupEntity.getModifications().removeAll(modifications); // No need to remove the group from the modification as we're going to delete it
                deleteModifications(modifications);
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

    private void deleteModifications(List<ModificationEntity> modificationEntities) {
        // This optimizes the treatment for tabular modifications but reduces efficiency for a list of 'unitary'
        // modifications. Nevertheless, for the volumes we are considering (max few hundreds) it is still very
        // efficient so no need to dig deeper about that for now.

        // delete tabular modification
        List<TabularModificationEntity> tabularModificationsToDelete = modificationEntities.stream().filter(TabularModificationEntity.class::isInstance).map(TabularModificationEntity.class::cast).toList();
        tabularModificationsToDelete.forEach(this::deleteTabularModification);

        // delete other modification types with "in" requests
        List<UUID> uuidsToDelete = modificationEntities.stream().filter(Predicate.not(TabularModificationEntity.class::isInstance)).map(ModificationEntity::getId).toList();
        modificationApplicationInfosService.deleteAllByModificationIds(uuidsToDelete);
        modificationRepository.deleteAllByIdIn(uuidsToDelete);
    }

    private void deleteTabularModification(TabularModificationEntity tabularModificationEntity) {
        List<UUID> modificationToCleanUuids = new ArrayList<>();
        modificationToCleanUuids.add(tabularModificationEntity.getId());
        switch (tabularModificationEntity.getModificationType()) {
            case GENERATOR_MODIFICATION:
                List<UUID> subModificationsIds = modificationRepository.findSubModificationIdsByTabularModificationId(tabularModificationEntity.getId());
                modificationToCleanUuids.addAll(subModificationsIds);
                modificationApplicationInfosService.deleteAllByModificationIds(modificationToCleanUuids);
                generatorModificationRepository.deleteTabularModification(subModificationsIds, tabularModificationEntity.getId());
                break;
            default:
                modificationApplicationInfosService.deleteAllByModificationIds(modificationToCleanUuids);
                modificationRepository.delete(tabularModificationEntity);
                break;
        }
    }
}
