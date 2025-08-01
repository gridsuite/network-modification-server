/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import lombok.NonNull;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
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

    private final LoadModificationRepository loadModificationRepository;
    private final GeneratorModificationRepository generatorModificationRepository;
    private final BatteryModificationRepository batteryModificationRepository;
    private final ShuntCompensatorModificationRepository shuntCompensatorModificationRepository;
    private final LineModificationRepository lineModificationRepository;
    private final TwoWindingsTransformerModificationRepository twoWindingsTransformerModificationRepository;
    private final SubstationModificationRepository substationModificationRepository;
    private final VoltageLevelModificationRepository voltageLevelModificationRepository;

    private final LoadCreationRepository loadCreationRepository;
    private final GeneratorCreationRepository generatorCreationRepository;
    private final BatteryCreationRepository batteryCreationRepository;
    private final ShuntCompensatorCreationRepository shuntCompensatorCreationRepository;

    private final TabularPropertyRepository tabularPropertyRepository;

    private final ModificationApplicationInfosService modificationApplicationInfosService;

    private static final String MODIFICATION_NOT_FOUND_MESSAGE = "Modification (%s) not found";

    public NetworkModificationRepository(ModificationGroupRepository modificationGroupRepository,
                                         ModificationRepository modificationRepository,
                                         GeneratorCreationRepository generatorCreationRepository,
                                         BatteryCreationRepository batteryCreationRepository,
                                         LoadCreationRepository loadCreationRepository,
                                         ShuntCompensatorCreationRepository shuntCompensatorCreationRepository,
                                         GeneratorModificationRepository generatorModificationRepository,
                                         BatteryModificationRepository batteryModificationRepository,
                                         LoadModificationRepository loadModificationRepository,
                                         ShuntCompensatorModificationRepository shuntCompensatorModificationRepository,
                                         LineModificationRepository lineModificationRepository,
                                         TwoWindingsTransformerModificationRepository twoWindingsTransformerModificationRepository,
                                         SubstationModificationRepository substationModificationRepository,
                                         VoltageLevelModificationRepository voltageLevelModificationRepository,
                                         TabularPropertyRepository tabularPropertyRepository,
                                         ModificationApplicationInfosService modificationApplicationInfosService) {
        this.modificationGroupRepository = modificationGroupRepository;
        this.modificationRepository = modificationRepository;
        this.generatorCreationRepository = generatorCreationRepository;
        this.batteryCreationRepository = batteryCreationRepository;
        this.loadCreationRepository = loadCreationRepository;
        this.shuntCompensatorCreationRepository = shuntCompensatorCreationRepository;
        this.generatorModificationRepository = generatorModificationRepository;
        this.batteryModificationRepository = batteryModificationRepository;
        this.loadModificationRepository = loadModificationRepository;
        this.shuntCompensatorModificationRepository = shuntCompensatorModificationRepository;
        this.lineModificationRepository = lineModificationRepository;
        this.twoWindingsTransformerModificationRepository = twoWindingsTransformerModificationRepository;
        this.substationModificationRepository = substationModificationRepository;
        this.voltageLevelModificationRepository = voltageLevelModificationRepository;
        this.tabularPropertyRepository = tabularPropertyRepository;
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

    private List<EquipmentModificationEntity> reorderModifications(List<? extends EquipmentModificationEntity> modifications, List<UUID> subModificationsOrderedUuids) {
        Map<UUID, EquipmentModificationEntity> modificationsMap = modifications.stream()
                .collect(Collectors.toMap(
                        ModificationEntity::getId,
                        Function.identity()
                ));
        return subModificationsOrderedUuids
                .stream()
                .map(modificationsMap::get)
                .toList();
    }

    private List<? extends EquipmentModificationEntity> loadTabularModificationSubEntities(List<UUID> subModificationsUuids, ModificationType modificationType) {
        List<? extends EquipmentModificationEntity> modifications;
        switch (modificationType) {
            case GENERATOR_MODIFICATION -> {
                // load generator modifications with curvePoints
                modifications = generatorModificationRepository.findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids).stream().toList();
                // load properties too, it uses hibernate first-level cache to fill them up directly in modifications
                generatorModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case BATTERY_MODIFICATION -> {
                // load battery modifications with curvePoints
                modifications = batteryModificationRepository.findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids).stream().toList();
                // load properties too, it uses hibernate first-level cache to fill them up directly in modifications
                batteryModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case LINE_MODIFICATION -> {
                // load line modifications with opLimitsGroups1
                modifications = lineModificationRepository.findAllOperationalLimitsGroups1ByIdIn(subModificationsUuids);
                // load opLimitsGroups2 too, it uses hibernate first-level cache to fill them up directly in modifications
                lineModificationRepository.findAllOperationalLimitsGroups2ByIdIn(subModificationsUuids);
                // same with properties
                lineModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> {
                // load 2wt modifications with opLimitsGroups1
                modifications = twoWindingsTransformerModificationRepository.findAllOperationalLimitsGroups1ByIdIn(subModificationsUuids);
                // load opLimitsGroups2 too, it uses hibernate first-level cache to fill them up directly in modifications
                twoWindingsTransformerModificationRepository.findAllOperationalLimitsGroups2ByIdIn(subModificationsUuids);
                // same with properties
                twoWindingsTransformerModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case LOAD_MODIFICATION ->
                // load Load modifications with properties
                modifications = loadModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            case SUBSTATION_MODIFICATION ->
                // load substation modifications with properties
                modifications = substationModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            case VOLTAGE_LEVEL_MODIFICATION ->
                // load VL modifications with properties
                modifications = voltageLevelModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            case SHUNT_COMPENSATOR_MODIFICATION ->
                // load MCS modifications with properties
                modifications = shuntCompensatorModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            default ->
                throw new UnsupportedOperationException(String.format("No sub-modifications loading for modification type: %s", modificationType));
        }
        return modifications;
    }

    private TabularModificationInfos loadTabularModification(ModificationEntity modificationEntity) {
        TabularModificationEntity tabularModificationEntity = (TabularModificationEntity) modificationEntity;
        // fetch embedded modifications uuids only
        List<UUID> subModificationsUuids = modificationRepository.findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(modificationEntity.getId());
        // optimized entities full loading, per type
        List<? extends EquipmentModificationEntity> modifications = loadTabularModificationSubEntities(subModificationsUuids, tabularModificationEntity.getModificationType());
        // re-order the list of entities based on the ordered list of IDs
        List<EquipmentModificationEntity> orderedModifications = reorderModifications(modifications, subModificationsUuids);
        // then build DTOs
        return TabularModificationInfos.builder()
                .uuid(tabularModificationEntity.getId())
                .date(tabularModificationEntity.getDate())
                .stashed(tabularModificationEntity.getStashed())
                .activated(tabularModificationEntity.getActivated())
                .modificationType(tabularModificationEntity.getModificationType())
                .modifications(orderedModifications.stream().map(ModificationEntity::toModificationInfos).toList())
                .properties(CollectionUtils.isEmpty(tabularModificationEntity.getProperties()) ? null : tabularModificationEntity.getProperties().stream()
                        .map(TabularPropertyEntity::toInfos)
                        .toList())
                .build();
    }

    private List<? extends EquipmentModificationEntity> loadTabularCreationSubEntities(List<UUID> subModificationsUuids, ModificationType modificationType) {
        List<? extends EquipmentModificationEntity> modifications;
        switch (modificationType) {
            case GENERATOR_CREATION -> {
                // load generator modifications with curvePoints
                modifications = generatorCreationRepository.findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids).stream().toList();
                // load properties too, it uses hibernate first-level cache to fill them up directly in modifications
                generatorCreationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case BATTERY_CREATION -> {
                // load battery modifications with curvePoints
                modifications = batteryCreationRepository.findAllReactiveCapabilityCurvePointsByIdIn(subModificationsUuids).stream().toList();
                // load properties too, it uses hibernate first-level cache to fill them up directly in modifications
                batteryCreationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case LOAD_CREATION ->
                // load Load modifications with properties
                modifications = loadCreationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            case SHUNT_COMPENSATOR_CREATION ->
                // load MCS modifications with properties
                modifications = shuntCompensatorCreationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            default ->
                throw new UnsupportedOperationException(String.format("No sub-modifications loading for creation type: %s", modificationType));
        }
        return modifications;
    }

    private TabularCreationInfos loadTabularCreation(ModificationEntity modificationEntity) {
        TabularCreationEntity tabularCreationEntity = (TabularCreationEntity) modificationEntity;
        // fetch embedded modifications uuids only
        List<UUID> subModificationsUuids = modificationRepository.findSubModificationIdsByTabularCreationIdOrderByModificationsOrder(modificationEntity.getId());
        // optimized entities full loading, per type
        List<? extends EquipmentModificationEntity> modifications = loadTabularCreationSubEntities(subModificationsUuids, tabularCreationEntity.getCreationType());
        // re-order the list of entities based on the ordered list of IDs
        List<EquipmentModificationEntity> orderedModifications = reorderModifications(modifications, subModificationsUuids);
        // then build DTOs
        return TabularCreationInfos.builder()
                .uuid(tabularCreationEntity.getId())
                .date(tabularCreationEntity.getDate())
                .stashed(tabularCreationEntity.getStashed())
                .activated(tabularCreationEntity.getActivated())
                .creationType(tabularCreationEntity.getCreationType())
                .creations(orderedModifications.stream().map(ModificationEntity::toModificationInfos).toList())
                .properties(CollectionUtils.isEmpty(tabularCreationEntity.getProperties()) ? null : tabularCreationEntity.getProperties().stream()
                        .map(TabularPropertyEntity::toInfos)
                        .toList())
                .build();
    }

    public ModificationInfos getModificationInfos(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof TabularModificationEntity) {
            return loadTabularModification(modificationEntity);
        } else if (modificationEntity instanceof TabularCreationEntity) {
            return loadTabularCreation(modificationEntity);
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
        ModificationEntity entity = getModificationEntity(modificationUuid);
        // Tabular modifications optimization:
        // Before updating/adding with new sub-modifications, we delete and clear existing sub-modifications manually
        // to avoid JPA to make a huge query to find them (no need to read them, they are going to be replaced).
        if (modificationInfos.getType() == ModificationType.TABULAR_MODIFICATION) {
            TabularModificationEntity tabularModificationEntity = (TabularModificationEntity) entity;
            deleteTabularModificationSubModifications(tabularModificationEntity);
            tabularModificationEntity.update(modificationInfos);
        } else if (modificationInfos.getType() == ModificationType.TABULAR_CREATION) {
            TabularCreationEntity tabularCreationEntity = (TabularCreationEntity) entity;
            deleteTabularCreationSubModifications(tabularCreationEntity);
            tabularCreationEntity.update(modificationInfos);
        } else {
            entity.update(modificationInfos);
        }
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

        // delete tabular modifications
        List<TabularModificationEntity> tabularModificationsToDelete = modificationEntities.stream().filter(TabularModificationEntity.class::isInstance).map(TabularModificationEntity.class::cast).toList();
        tabularModificationsToDelete.forEach(this::deleteTabularModification);
        List<TabularCreationEntity> tabularCreationsToDelete = modificationEntities.stream().filter(TabularCreationEntity.class::isInstance).map(TabularCreationEntity.class::cast).toList();
        tabularCreationsToDelete.forEach(this::deleteTabularCreation);

        // delete other modification types with "in" requests
        List<UUID> uuidsToDelete = modificationEntities.stream().filter(Predicate.not(TabularModificationEntity.class::isInstance)).map(ModificationEntity::getId).toList();
        if (!uuidsToDelete.isEmpty()) {
            modificationApplicationInfosService.deleteAllByModificationIds(uuidsToDelete);
            modificationRepository.deleteAllByIdIn(uuidsToDelete);
        }
    }

    private void deleteTabularCreation(TabularCreationEntity tabularCreationEntity) {
        deleteTabular(tabularCreationEntity.getCreationType(), tabularCreationEntity);
    }

    private void deleteTabularModification(TabularModificationEntity tabularModificationEntity) {
        deleteTabular(tabularModificationEntity.getModificationType(), tabularModificationEntity);
    }

    private void deleteTabular(ModificationType tabularModificationType, ModificationEntity modificationEntity) {
        UUID modificationUuid = modificationEntity.getId();
        List<UUID> modificationToCleanUuids = new ArrayList<>();
        modificationToCleanUuids.add(modificationUuid);
        List<UUID> subModificationsIds = modificationEntity instanceof TabularModificationEntity ?
                modificationRepository.findSubModificationIdsByTabularModificationId(modificationUuid) :
                modificationRepository.findSubModificationIdsByTabularCreationId(modificationUuid);
        modificationToCleanUuids.addAll(subModificationsIds);
        modificationApplicationInfosService.deleteAllByModificationIds(modificationToCleanUuids);
        tabularPropertyRepository.deleteTabularProperties(modificationUuid);

        switch (tabularModificationType) {
            case GENERATOR_CREATION ->
                generatorCreationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case LOAD_CREATION ->
                loadCreationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case SHUNT_COMPENSATOR_CREATION ->
                shuntCompensatorCreationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case BATTERY_CREATION ->
                batteryCreationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case GENERATOR_MODIFICATION ->
                generatorModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case LOAD_MODIFICATION ->
                loadModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case SHUNT_COMPENSATOR_MODIFICATION ->
                shuntCompensatorModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case BATTERY_MODIFICATION ->
                batteryModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case VOLTAGE_LEVEL_MODIFICATION ->
                voltageLevelModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            case LINE_MODIFICATION -> {
                List<UUID> opLimitsGroups1Ids = modificationRepository.findLineModificationOpLimitsGroups1IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroups2Ids = modificationRepository.findLineModificationOpLimitsGroups2IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroupsIds = CollectionUtils.union(opLimitsGroups1Ids, opLimitsGroups2Ids).stream().toList();
                List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
                lineModificationRepository.deleteTabularModification(currentLimitsIds, opLimitsGroupsIds, subModificationsIds, modificationUuid);
            }
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> {
                List<UUID> opLimitsGroups1Ids = modificationRepository.findTwtModificationOpLimitsGroups1IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroups2Ids = modificationRepository.findTwtModificationOpLimitsGroups2IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroupsIds = CollectionUtils.union(opLimitsGroups1Ids, opLimitsGroups2Ids).stream().toList();
                List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
                twoWindingsTransformerModificationRepository.deleteTabularModification(currentLimitsIds, opLimitsGroupsIds, subModificationsIds, modificationUuid);
            }
            case SUBSTATION_MODIFICATION ->
                substationModificationRepository.deleteTabularModification(subModificationsIds, modificationUuid);
            default ->
                throw new UnsupportedOperationException(String.format("No modification full deletion for type: %s", tabularModificationType));
        }
    }

    private void deleteTabularCreationSubModifications(TabularCreationEntity tabularCreationEntity) {
        ModificationType tabularCreationType = tabularCreationEntity.getCreationType();
        UUID modificationId = tabularCreationEntity.getId();
        List<UUID> subModificationsIds = modificationRepository.findSubModificationIdsByTabularCreationId(modificationId);
        tabularCreationEntity.setCreations(null);
        modificationApplicationInfosService.deleteAllByModificationIds(subModificationsIds);
        switch (tabularCreationType) {
            case GENERATOR_CREATION ->
                generatorCreationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case LOAD_CREATION ->
                loadCreationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case SHUNT_COMPENSATOR_CREATION ->
                shuntCompensatorCreationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case BATTERY_CREATION ->
                batteryCreationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            default ->
                throw new UnsupportedOperationException(String.format("No sub-modifications deletion for creation type: %s", tabularCreationType));
        }
    }

    private void deleteTabularModificationSubModifications(TabularModificationEntity tabularModificationEntity) {
        ModificationType tabularModificationType = tabularModificationEntity.getModificationType();
        UUID modificationId = tabularModificationEntity.getId();
        List<UUID> subModificationsIds = modificationRepository.findSubModificationIdsByTabularModificationId(modificationId);
        tabularModificationEntity.setModifications(null);
        modificationApplicationInfosService.deleteAllByModificationIds(subModificationsIds);
        switch (tabularModificationType) {
            case GENERATOR_MODIFICATION ->
                generatorModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case LOAD_MODIFICATION ->
                loadModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case SHUNT_COMPENSATOR_MODIFICATION ->
                shuntCompensatorModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case BATTERY_MODIFICATION ->
                batteryModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case VOLTAGE_LEVEL_MODIFICATION ->
                voltageLevelModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            case LINE_MODIFICATION -> {
                List<UUID> opLimitsGroups1Ids = modificationRepository.findLineModificationOpLimitsGroups1IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroups2Ids = modificationRepository.findLineModificationOpLimitsGroups2IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroupsIds = CollectionUtils.union(opLimitsGroups1Ids, opLimitsGroups2Ids).stream().toList();
                List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
                lineModificationRepository.deleteTabularSubModifications(currentLimitsIds, opLimitsGroupsIds, subModificationsIds, modificationId);
            }
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> {
                List<UUID> opLimitsGroups1Ids = modificationRepository.findTwtModificationOpLimitsGroups1IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroups2Ids = modificationRepository.findTwtModificationOpLimitsGroups2IdsByBranchIds(subModificationsIds);
                List<UUID> opLimitsGroupsIds = CollectionUtils.union(opLimitsGroups1Ids, opLimitsGroups2Ids).stream().toList();
                List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
                twoWindingsTransformerModificationRepository.deleteTabularSubModifications(currentLimitsIds, opLimitsGroupsIds, subModificationsIds, modificationId);
            }
            case SUBSTATION_MODIFICATION ->
                substationModificationRepository.deleteTabularSubModifications(subModificationsIds, modificationId);
            default ->
                throw new UnsupportedOperationException(String.format("No sub-modifications deletion for modification type: %s", tabularModificationType));
        }
    }
}
