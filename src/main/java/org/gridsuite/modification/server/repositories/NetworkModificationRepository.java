/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.google.common.collect.Lists;
import lombok.NonNull;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.dto.tabular.*;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.tabular.TabularModificationsEntity;
import org.gridsuite.modification.server.entities.tabular.TabularPropertyEntity;
import org.gridsuite.modification.server.error.NetworkModificationGroupNotFoundException;
import org.gridsuite.modification.server.error.NetworkModificationServerRunException;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.apache.commons.collections4.SetUtils.emptyIfNull;
import static org.gridsuite.modification.server.utils.DatabaseConstants.SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE;
import static org.gridsuite.modification.server.utils.DatabaseConstants.SQL_SUB_MODIFICATION_WITH_LIMITSET_DELETION_BATCH_SIZE;

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
    // This method should be package-private and not used as API of the service as it uses ModificationEntity and
    // we want to encapsulate the use of Entity related objects to this service.
    // Nevertheless We have to keep it public for transactional annotation.
    public List<ModificationInfos> saveModifications(UUID groupUuid, List<ModificationEntity> modifications) {
        List<ModificationEntity> entities = saveModificationsNonTransactional(groupUuid, modifications);
        return entities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    @Transactional
    public List<ModificationInfos> saveModificationInfos(UUID groupUuid, List<ModificationInfos> modifications) {
        List<ModificationEntity> entities = saveModificationInfosNonTransactional(groupUuid, modifications);
        // We can't return input modifications directly because it wouldn't have the IDs coming from the saved entities
        return entities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    private List<ModificationEntity> saveModificationInfosNonTransactional(UUID groupUuid,
                                                                           List<ModificationInfos> modifications) {
        List<ModificationEntity> entities = modifications.stream().map(ModificationEntity::fromDTO).toList();

        return saveModificationsNonTransactional(groupUuid, entities);
    }

    public UUID createNetworkCompositeModification(@NonNull List<UUID> modificationUuids) {
        CompositeModificationInfos compositeInfos = CompositeModificationInfos.builder().modifications(List.of()).build();
        CompositeModificationEntity compositeEntity = (CompositeModificationEntity) ModificationEntity.fromDTO(compositeInfos);
        List<ModificationEntity> copyEntities = modificationRepository.findAllByIdIn(modificationUuids).stream()
            .map(this::toModificationsInfosOptimizedForTabular)
            .map(ModificationEntity::fromDTO)
            .toList();
        compositeEntity.setModifications(copyEntities);
        return modificationRepository.save(compositeEntity).getId();
    }

    public void updateCompositeModification(@NonNull UUID compositeUuid, @NonNull List<UUID> modificationUuids) {
        ModificationEntity modificationEntity = modificationRepository.findById(compositeUuid)
            .orElseThrow(() -> new NetworkModificationServerRunException(String.format(MODIFICATION_NOT_FOUND_MESSAGE, compositeUuid)));

        if (!(modificationEntity instanceof CompositeModificationEntity compositeEntity)) {
            throw new NetworkModificationServerRunException(String.format("Modification (%s) is not a composite modification", compositeUuid));
        }

        List<ModificationEntity> copyEntities = modificationRepository.findAllByIdIn(modificationUuids).stream()
            .map(this::toModificationsInfosOptimizedForTabular)
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
    public List<ModificationInfos> moveModifications(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsToMoveUUID, UUID referenceModificationUuid) {
        List<ModificationEntity> movedModifications = moveModificationsNonTransactional(destinationGroupUuid, originGroupUuid, modificationsToMoveUUID, referenceModificationUuid);
        return movedModifications.stream().map(this::toModificationsInfosOptimizedForTabular).toList();
    }

    private List<ModificationEntity> moveModificationsNonTransactional(UUID destinationGroupUuid, UUID originGroupUuid, List<UUID> modificationsToMoveUUID, UUID referenceModificationUuid) {
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
            .orElseThrow(() -> new NetworkModificationServerRunException("Insert modification error"));
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
            .map(this::toModificationsInfosOptimizedForTabular)
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
            return onlyMetadata ? getModificationsMetadata(groupUuid, onlyStashed) : getModificationsInfos(List.of(groupUuid), onlyStashed);
        } catch (NetworkModificationGroupNotFoundException e) {
            if (!errorOnGroupNotFound) {
                return List.of();
            } else {
                throw e;
            }
        }
    }

    public List<ModificationInfos> getModificationsMetadata(UUID groupUuid, boolean onlyStashed) {
        if (onlyStashed) {
            return modificationRepository
                .findAllBaseByGroupIdReverse(getModificationGroup(groupUuid).getId())
                .stream()
                .filter(ModificationEntity::getStashed)
                .map(this::toModificationsInfosOptimizedForTabular)
                .collect(Collectors.toList());
        } else {
            return modificationRepository
                .findAllBaseByGroupId(getModificationGroup(groupUuid).getId())
                .stream()
                .map(this::toModificationsInfosOptimizedForTabular)
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
                // load line modifications with opLimitsGroups
                modifications = lineModificationRepository.findAllOperationalLimitsGroupsByIdIn(subModificationsUuids);
                // same with properties, it uses hibernate first-level cache to fill them up directly in modifications
                lineModificationRepository.findAllPropertiesByIdIn(subModificationsUuids);
            }
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION -> {
                // load 2wt modifications with opLimitsGroups
                modifications = twoWindingsTransformerModificationRepository.findAllOperationalLimitsGroupsByIdIn(subModificationsUuids);
                // same with properties, it uses hibernate first-level cache to fill them up directly in modifications
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

    private TabularBaseInfos loadTabularModification(TabularModificationsEntity tabularEntity) {
        // fetch embedded modifications uuids only
        List<UUID> subModificationsUuids = modificationRepository.findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(tabularEntity.getId());
        // optimized entities full loading, per type
        List<? extends EquipmentModificationEntity> modifications = loadTabularModificationSubEntities(subModificationsUuids, tabularEntity.getModificationType());
        // re-order the list of entities based on the ordered list of IDs
        List<EquipmentModificationEntity> orderedModifications = reorderModifications(modifications, subModificationsUuids);
        var builder = switch (ModificationType.valueOf(tabularEntity.getType())) {
            case ModificationType.TABULAR_CREATION -> TabularCreationInfos.builder();
            case ModificationType.LIMIT_SETS_TABULAR_MODIFICATION -> LimitSetsTabularModificationInfos.builder();
            default -> TabularModificationInfos.builder();
        };
        return builder.uuid(tabularEntity.getId())
            .date(tabularEntity.getDate())
            .stashed(tabularEntity.getStashed())
            .activated(tabularEntity.getActivated())
            .modificationType(tabularEntity.getModificationType())
            .modifications(orderedModifications.stream().map(ModificationEntity::toModificationInfos).toList())
            .properties(CollectionUtils.isEmpty(tabularEntity.getProperties()) ? null : tabularEntity.getProperties().stream()
                .map(TabularPropertyEntity::toInfos)
                .toList())
            .csvFilename(tabularEntity.getCsvFilename())
            .build();
    }

    public ModificationInfos toModificationsInfosOptimizedForTabular(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof TabularModificationsEntity tabularEntity) {
            return loadTabularModification(tabularEntity);
        }
        return modificationEntity.toModificationInfos();
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getActiveModifications(UUID groupUuid, Set<UUID> modificationsToExclude) {
        List<ModificationEntity> modificationsEntities = modificationRepository.findAllActiveModificationsByGroupId(groupUuid, emptyIfNull(modificationsToExclude));
        return modificationsEntities.stream().map(this::toModificationsInfosOptimizedForTabular).toList();
    }

    private List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids, boolean onlyStashed) {
        return groupUuids.stream().flatMap(this::getModificationEntityStream)
            .filter(m -> !onlyStashed || m.getStashed() == onlyStashed)
            .map(this::toModificationsInfosOptimizedForTabular).toList();
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return toModificationsInfosOptimizedForTabular(getModificationEntity(modificationUuid));
    }

    public ModificationEntity getModificationEntity(UUID modificationUuid) {
        return modificationRepository
            .findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationServerRunException("Modification not found: " + modificationUuid.toString()));
    }

    @Transactional // To have the 2 delete in the same transaction (atomic)
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                //TODO: is there a way to avoid doing this setGroup(null) that triggers a useless update since the entity will be deleted right after
                groupEntity.getModifications().forEach(modif -> modif.setGroup(null));
                List<ModificationEntity> modifications = groupEntity.getModifications();
                deleteModifications(modifications.stream().filter(Objects::nonNull).toList());
            }
            modificationGroupRepository.delete(groupEntity);
        } catch (NetworkModificationGroupNotFoundException e) {
            if (errorOnGroupNotFound) {
                throw e;
            }
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
                throw new NetworkModificationServerRunException(String.format("Modification deletion error: %s is owned by group %s",
                    optionalModificationWithGroup.get().getId().toString(), optionalModificationWithGroup.get().getGroup().getId()));
            }
        } else {
            throw new NetworkModificationServerRunException("Modification deletion error: need to specify the group or give a list of UUIDs");
        }
        int count = modifications.size();
        deleteModifications(modifications);
        return count;
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid).orElseThrow(() -> new NetworkModificationGroupNotFoundException("Modification Group not found " + groupUuid));
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

    private List<ModificationInfos> getModificationsInfosNonTransactional(List<UUID> uuids) {
        // Spring-data findAllById doc says: the order of elements in the result is not guaranteed
        Map<UUID, ModificationEntity> entities = modificationRepository.findAllById(uuids)
            .stream()
            .collect(Collectors.toMap(
                ModificationEntity::getId,
                Function.identity()
            ));
        return uuids.stream().map(entities::get).filter(Objects::nonNull).map(this::toModificationsInfosOptimizedForTabular).toList();
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
            .map(this::toModificationsInfosOptimizedForTabular)
            .toList();
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getCompositeModificationsInfos(@NonNull List<UUID> uuids) {
        return getCompositeModificationsInfosNonTransactional(uuids);
    }

    private List<ModificationInfos> getCompositeModificationsInfosNonTransactional(@NonNull List<UUID> uuids) {
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
    public List<ModificationInfos> getUnstashedModificationsInfos(@NonNull UUID groupUuid) {
        return getUnstashedModificationsInfosNonTransactional(groupUuid);
    }

    private List<ModificationInfos> getUnstashedModificationsInfosNonTransactional(UUID groupUuid) {
        return getModificationEntityStream(groupUuid).filter(m -> !m.getStashed()).map(this::toModificationsInfosOptimizedForTabular).toList();
    }

    @Transactional
    public void stashNetworkModifications(@NonNull List<UUID> modificationUuids, int stashedModificationCount) {
        int stashModificationOrder = -stashedModificationCount - 1;
        List<ModificationEntity> modificationEntities = new ArrayList<>();
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                .findById(modificationUuid)
                .orElseThrow(() -> new NetworkModificationServerRunException(String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
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
            throw new NetworkModificationServerRunException("Modification not found");
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
                .orElseThrow(() -> new NetworkModificationServerRunException(String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            modificationEntity.setActivated(activated);
        }
    }

    @Transactional
    public void updateModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        ModificationEntity entity = getModificationEntity(modificationUuid);
        // Tabular modifications optimization:
        // Before updating/adding with new sub-modifications, we delete and clear existing sub-modifications manually
        // to avoid JPA to make a huge query to find them (no need to read them, they are going to be replaced).
        if (modificationInfos.getType() == ModificationType.TABULAR_CREATION
            || modificationInfos.getType() == ModificationType.TABULAR_MODIFICATION
            || modificationInfos.getType() == ModificationType.LIMIT_SETS_TABULAR_MODIFICATION) {
            TabularModificationsEntity tabularEntity = (TabularModificationsEntity) entity;
            deleteTabularModificationSubModifications(tabularEntity);
            tabularEntity.update(modificationInfos);
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
        } catch (NetworkModificationGroupNotFoundException e) {
            if (errorOnGroupNotFound) {
                throw e;
            }
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

        // delete tabular modifications/creations
        List<TabularModificationsEntity> tabularModificationsToDelete = modificationEntities.stream().filter(TabularModificationsEntity.class::isInstance).map(TabularModificationsEntity.class::cast).toList();
        tabularModificationsToDelete.forEach(this::deleteTabularModification);

        // delete other modification types with "in" requests
        List<UUID> uuidsToDelete = modificationEntities.stream().filter(Predicate.not(TabularModificationsEntity.class::isInstance)).map(ModificationEntity::getId).toList();
        if (!uuidsToDelete.isEmpty()) {
            modificationApplicationInfosService.deleteAllByModificationIds(uuidsToDelete);
            modificationRepository.deleteAllByIdIn(uuidsToDelete);
        }
    }

    private void deleteSomeLineTabularSubModifications(List<UUID> subModificationsIds) {
        List<UUID> opLimitsGroupsIds = modificationRepository.findLineModificationOpLimitsGroupsIdsByBranchIds(subModificationsIds);
        List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
        lineModificationRepository.deleteSomeTabularSubModifications(currentLimitsIds, opLimitsGroupsIds, subModificationsIds);
    }

    private void deleteSomeTwtTabularSubModifications(List<UUID> subModificationsIds) {
        List<UUID> opLimitsGroupsIds = modificationRepository.findTwtModificationOpLimitsGroupsIdsByBranchIds(subModificationsIds);
        List<UUID> currentLimitsIds = modificationRepository.findCurrentLimitsIdsByOpLimitsGroupsIds(opLimitsGroupsIds);
        twoWindingsTransformerModificationRepository.deleteSomeTabularSubModifications(currentLimitsIds, opLimitsGroupsIds, subModificationsIds);
    }

    private void deleteAllTabularSubModificationsUsingPartition(ModificationType tabularModificationType, List<UUID> subModificationsIds) {
        switch (tabularModificationType) {
            case GENERATOR_CREATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(generatorCreationRepository::deleteSomeTabularSubModifications);
            case LOAD_CREATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(loadCreationRepository::deleteSomeTabularSubModifications);
            case SHUNT_COMPENSATOR_CREATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(shuntCompensatorCreationRepository::deleteSomeTabularSubModifications);
            case BATTERY_CREATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(batteryCreationRepository::deleteSomeTabularSubModifications);
            case GENERATOR_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(generatorModificationRepository::deleteSomeTabularSubModifications);
            case LOAD_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(loadModificationRepository::deleteSomeTabularSubModifications);
            case SHUNT_COMPENSATOR_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(shuntCompensatorModificationRepository::deleteSomeTabularSubModifications);
            case BATTERY_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(batteryModificationRepository::deleteSomeTabularSubModifications);
            case VOLTAGE_LEVEL_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(voltageLevelModificationRepository::deleteSomeTabularSubModifications);
            case LINE_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_WITH_LIMITSET_DELETION_BATCH_SIZE).forEach(this::deleteSomeLineTabularSubModifications);
            case TWO_WINDINGS_TRANSFORMER_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_WITH_LIMITSET_DELETION_BATCH_SIZE).forEach(this::deleteSomeTwtTabularSubModifications);
            case SUBSTATION_MODIFICATION ->
                Lists.partition(subModificationsIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).forEach(substationModificationRepository::deleteSomeTabularSubModifications);
            default ->
                throw new UnsupportedOperationException(String.format("No sub-modifications deletion method for type: %s", tabularModificationType));
        }
    }

    private void deleteTabularModification(TabularModificationsEntity tabularEntity) {
        UUID modificationUuid = tabularEntity.getId();
        List<UUID> modificationToCleanUuids = new ArrayList<>();
        modificationToCleanUuids.add(modificationUuid);
        List<UUID> subModificationsIds = modificationRepository.findSubModificationIdsByTabularModificationId(modificationUuid);
        modificationToCleanUuids.addAll(subModificationsIds);
        modificationApplicationInfosService.deleteAllByModificationIds(modificationToCleanUuids);
        tabularPropertyRepository.deleteTabularProperties(modificationUuid);
        deleteAllTabularSubModificationsUsingPartition(tabularEntity.getModificationType(), subModificationsIds);
        // line functions work for any type
        lineModificationRepository.deleteTabularModificationModifications(modificationUuid, subModificationsIds);
        lineModificationRepository.deleteTabularModificationItself(modificationUuid);
    }

    private void deleteTabularModificationSubModifications(TabularModificationsEntity tabularModificationEntity) {
        UUID modificationId = tabularModificationEntity.getId();
        List<UUID> subModificationsIds = modificationRepository.findSubModificationIdsByTabularModificationId(modificationId);
        tabularModificationEntity.setModifications(null);
        modificationApplicationInfosService.deleteAllByModificationIds(subModificationsIds);
        deleteAllTabularSubModificationsUsingPartition(tabularModificationEntity.getModificationType(), subModificationsIds);
        // line function works for any type
        lineModificationRepository.deleteTabularModificationModifications(modificationId, subModificationsIds);
    }

    @Transactional
    public List<ModificationInfos> saveDuplicateModifications(@NonNull UUID targetGroupUuid, UUID originGroupUuid, @NonNull List<UUID> modificationsUuids) {
        List<ModificationInfos> modificationInfos = originGroupUuid != null ? getUnstashedModificationsInfosNonTransactional(originGroupUuid) : getModificationsInfosNonTransactional(modificationsUuids);
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, modificationInfos);
        // We can't return modificationInfos directly because it wouldn't have the IDs coming from the new saved entities
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    @Transactional
    public List<ModificationInfos> saveCompositeModifications(@NonNull UUID targetGroupUuid, @NonNull List<UUID> modificationsUuids) {
        List<ModificationInfos> modificationInfos = getCompositeModificationsInfosNonTransactional(modificationsUuids);
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, modificationInfos);
        // We can't return modificationInfos directly because it wouldn't have the IDs coming from the new saved entities
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }
}
