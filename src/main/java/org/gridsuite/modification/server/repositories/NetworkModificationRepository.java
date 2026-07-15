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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularBaseInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.server.dto.CompositeInfos;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.tabular.TabularModificationsEntity;
import org.gridsuite.modification.server.entities.tabular.TabularPropertyEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.apache.commons.collections4.SetUtils.emptyIfNull;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
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
    private final CompositeModificationRepository compositeModificationRepository;

    private final TabularPropertyRepository tabularPropertyRepository;
    private final ModificationContainerRepository modificationContainerRepository;
    private final CompositeContainerRepository compositeContainerRepository;

    private final ModificationApplicationInfosService modificationApplicationInfosService;

    private static final String MODIFICATION_NOT_FOUND_MESSAGE = "Modification (%s) not found";

    private static final Logger LOGGER = LoggerFactory.getLogger(NetworkModificationRepository.class);

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
                                         CompositeModificationRepository compositeModificationRepository,
                                         CompositeContainerRepository compositeContainerRepository,
                                         ModificationContainerRepository modificationContainerRepository,
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
        this.compositeModificationRepository = compositeModificationRepository;
        this.compositeContainerRepository = compositeContainerRepository;
        this.modificationContainerRepository = modificationContainerRepository;
        this.modificationApplicationInfosService = modificationApplicationInfosService;
    }

    @Transactional // To have all the delete in the same transaction (atomic)
    public void deleteAll() {
        modificationApplicationInfosService.deleteAll();
        modificationRepository.deleteAll();
        compositeContainerRepository.deleteAll();
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
    public List<ModificationInfos> saveModificationInfos(@NonNull UUID groupUuid, List<ModificationInfos> modifications) {
        List<ModificationEntity> entities = saveModificationInfosNonTransactional(groupUuid, modifications);
        // We can't return input modifications directly because it wouldn't have the IDs coming from the saved entities
        return entities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    private List<ModificationEntity> saveModificationInfosNonTransactional(@NonNull UUID groupUuid, List<ModificationInfos> modifications) {
        List<ModificationEntity> entities = modifications.stream().map(ModificationEntity::fromDTO).toList();
        return saveModificationsNonTransactional(groupUuid, entities);
    }

    public UUID createNetworkCompositeModification(@NonNull List<UUID> modificationUuids, @NonNull String name) {
        // Fetch originals once, preserving order
        Map<UUID, ModificationEntity> cloneByUuid = modificationRepository.findAllByIdIn(modificationUuids).stream()
                .collect(Collectors.toMap(
                        ModificationEntity::getId,
                        e -> ModificationEntity.fromDTO(toModificationsInfosOptimized(e))
                ));
        // Reorder clones to match caller-specified order
        List<ModificationEntity> copyEntities = modificationUuids.stream()
                .map(cloneByUuid::get)
                .filter(Objects::nonNull)
                .toList();

        //TODO : separate creation and copy
        if (copyEntities.size() == 1 && copyEntities.getFirst() instanceof CompositeModificationEntity single) {
            return modificationRepository.save(single).getId();
        }

        CompositeModificationInfos compositeInfos = CompositeModificationInfos.builder().modificationsInfos(List.of()).name(name).build();
        CompositeModificationEntity compositeEntity = (CompositeModificationEntity) ModificationEntity.fromDTO(compositeInfos);
        compositeEntity.setModifications(copyEntities);
        return modificationRepository.save(compositeEntity).getId();
    }

    public void replaceCompositeModification(@NonNull UUID compositeUuid, @NonNull String name, @NonNull List<UUID> modificationUuids) {
        CompositeModificationEntity compositeEntity = compositeModificationRepository.findById(compositeUuid)
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, compositeUuid)));

        // Fetch originals once, preserving order
        Map<UUID, ModificationEntity> cloneByUuid = modificationRepository.findAllByIdIn(modificationUuids).stream()
                .collect(Collectors.toMap(
                        ModificationEntity::getId,
                        e -> ModificationEntity.fromDTO(toModificationsInfosOptimized(e))
                ));

        //Delete previously held modifications
        deleteModifications(compositeEntity.getModifications());

        // Reorder clones to match caller-specified order
        List<ModificationEntity> copyEntities = modificationUuids.stream()
                .map(cloneByUuid::get)
                .filter(Objects::nonNull)
                .toList();
        deleteCompositeChildrenSubtree(List.of(compositeEntity));
        compositeEntity.setModifications(copyEntities);
        compositeModificationRepository.renameCompositeModification(compositeEntity, name);
    }

    public void updateCompositeModification(@NonNull UUID compositeUuid, String name) {
        CompositeModificationEntity compositeEntity = compositeModificationRepository.findById(compositeUuid)
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, compositeUuid)));
        if (name != null) {
            compositeModificationRepository.renameCompositeModification(compositeEntity, name);
        }
    }

    private List<ModificationEntity> saveModificationsNonTransactional(@NonNull UUID groupUuid, List<ModificationEntity> modifications) {
        int order = modificationRepository.countByContainerAndStashed(groupUuid, false);
        ModificationGroupEntity group = getOrCreateModificationGroup(groupUuid);
        for (ModificationEntity m : modifications) {
            group.addModification(m, order++);
        }
        // persisting composite leaves cascades their content (and its modification_container row)
        return modificationRepository.saveAll(modifications);
    }

    @Transactional
    public List<ModificationInfos> moveModifications(
            @NonNull ModificationContainerType sourceType, @NonNull UUID sourceId,
            @NonNull ModificationContainerType targetType, @NonNull UUID targetId,
            @NonNull List<UUID> modificationUuids, UUID beforeModificationUuid) {
        return moveModificationsNonTransactional(sourceType, sourceId, targetType, targetId, modificationUuids, beforeModificationUuid)
                .stream().map(this::toModificationsInfosOptimized).toList();
    }

    private List<ModificationEntity> moveModificationsNonTransactional(
            ModificationContainerType sourceType, UUID sourceId,
            ModificationContainerType targetType, UUID targetId,
            List<UUID> modificationUuids, UUID beforeModificationUuid) {

        boolean sameContainer = sourceType == targetType && sourceId.equals(targetId);
        ModificationContainer source = resolveContainer(sourceType, sourceId, false);
        ModificationContainer target = sameContainer ? source : resolveContainer(targetType, targetId, true);

        List<ModificationEntity> sourceChildren = source.getModifications().stream()
                .filter(Objects::nonNull)
                .filter(m -> !Boolean.TRUE.equals(m.getStashed()))
                .collect(Collectors.toCollection(ArrayList::new));

        List<ModificationEntity> moved = removeModifications(sourceChildren, modificationUuids);
        if (moved.isEmpty()) {
            return List.of();
        }

        if (targetType == ModificationContainerType.COMPOSITE) {
            assertNoCompositeCycle(moved, targetId);
        }

        if (sameContainer) {
            insertModifications(sourceChildren, moved, beforeModificationUuid);
            source.setModifications(sourceChildren);
            return moved;
        }

        if (sourceType == ModificationContainerType.GROUP) {
            modificationApplicationInfosService.deleteAllByModificationIds(collectAllModificationUuids(moved));
        }

        List<ModificationEntity> targetChildren = new ArrayList<>(target.getModifications());
        targetChildren.removeIf(Objects::isNull);
        insertModifications(targetChildren, moved, beforeModificationUuid);
        target.setModifications(targetChildren);
        source.setModifications(sourceChildren);
        return moved;
    }

    private void assertNoCompositeCycle(List<ModificationEntity> moving, UUID targetCompositeLeafId) {
        for (ModificationEntity m : moving) {
            if (m instanceof CompositeModificationEntity movingComposite
                    && (movingComposite.getId().equals(targetCompositeLeafId)
                    || isInsideComposite(movingComposite, targetCompositeLeafId))) {
                throw new NetworkModificationException(MOVE_MODIFICATION_ERROR,
                        String.format("Moving composite (%s) into (%s) would create a cycle",
                                m.getId(), targetCompositeLeafId));
            }
        }
    }

    private boolean isInsideComposite(CompositeModificationEntity composite, UUID targetId) {
        for (ModificationEntity sub : composite.getModifications()) {
            if (sub.getId().equals(targetId)) {
                return true;
            }
            if (sub instanceof CompositeModificationEntity subComposite && isInsideComposite(subComposite, targetId)) {
                return true;
            }
        }
        return false;
    }

    private List<UUID> collectAllModificationUuids(List<ModificationEntity> entities) {
        List<UUID> uuids = new ArrayList<>();
        for (ModificationEntity entity : entities) {
            uuids.add(entity.getId());
            if (entity instanceof CompositeModificationEntity composite) {
                uuids.addAll(collectAllModificationUuids(composite.getModifications()));
            }
        }
        return uuids;
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
                .map(this::toModificationsInfosOptimized)
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
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    public List<ModificationInfos> getModificationsMetadata(UUID groupUuid, boolean onlyStashed) {
        UUID groupId = getModificationGroup(groupUuid).getId();
        List<ModificationEntity> base = onlyStashed
                ? modificationRepository.findAllBaseByContainerIdReverse(groupId)
                : modificationRepository.findAllBaseByContainerId(groupId);
        // TODO : move depth handling in specific code for composite
        Map<UUID, Integer> depths = batchCompositeDepths(base);
        return base.stream()
                .filter(m -> !onlyStashed || m.getStashed())
                .map(m -> toModificationMetadataInfos(m, depths))
                .toList();
    }

    private Map<UUID, Integer> batchCompositeDepths(Collection<ModificationEntity> entities) {
        List<UUID> compositeIds = entities.stream()
                .filter(e -> ModificationType.COMPOSITE_MODIFICATION.name().equals(e.getType()))
                .map(ModificationEntity::getId)
                .toList();
        return getCompositesMaxDepthMap(compositeIds);
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
                .description(tabularEntity.getDescription())
                .modificationType(tabularEntity.getModificationType())
                .modifications(orderedModifications.stream().map(ModificationEntity::toModificationInfos).toList())
                .properties(CollectionUtils.isEmpty(tabularEntity.getProperties()) ? null : tabularEntity.getProperties().stream()
                        .map(TabularPropertyEntity::toInfos)
                        .toList())
                .csvFilename(tabularEntity.getCsvFilename())
                .build();
    }

    private void prefetchCompositeSubTree(CompositeModificationEntity compositeEntity) {
        // Constant query count for the whole subtree: one recursive CTE + one fetch-join load.
        List<UUID> compositeUuids = new ArrayList<>(modificationRepository.findOnlyCompositeChildrenUuids(compositeEntity.getId()));
        compositeUuids.add(compositeEntity.getId());
        modificationRepository.findAllCompositesWithModificationsByIdIn(compositeUuids);
    }

    private CompositeModificationInfos loadCompositeModification(CompositeModificationEntity compositeEntity,
                                                                 Set<UUID> modificationsToExclude) {
        return CompositeModificationInfos.builder()
                .name(compositeEntity.getName())
                .activated(compositeEntity.getActivated())
                .description(compositeEntity.getDescription())
                .date(compositeEntity.getDate())
                .uuid(compositeEntity.getId())
                .stashed(compositeEntity.getStashed())
                .modificationsInfos(
                        compositeEntity.getModifications()
                                .stream()
                                .filter(m -> !modificationsToExclude.contains(m.getId()))
                                .map(m -> toModificationsInfosOptimized(m, modificationsToExclude, false))
                                .toList())
                .build();
    }

    private CompositeModificationInfos loadCompositeModificationMetadata(ModificationEntity compositeEntity, Integer maxDepth) {
        return CompositeModificationInfos.builder()
                .activated(compositeEntity.getActivated())
                .description(compositeEntity.getDescription())
                .date(compositeEntity.getDate())
                .uuid(compositeEntity.getId())
                .stashed(compositeEntity.getStashed())
                .messageType(compositeEntity.getMessageType())
                .messageValues(compositeEntity.getMessageValues())
                .maxDepth(maxDepth)
                .build();
    }

    private ModificationInfos loadModificationReference(ModificationEntity modificationEntity) {
        if (modificationEntity instanceof ModificationReferenceEntity referenceEntity) {
            ModificationEntity referencedEntity = modificationRepository.findAllByIdIn(List.of(referenceEntity.getReferenceId())).stream().findFirst()
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, referenceEntity.getReferenceId())));
            ModificationReferenceInfos modificationReferenceInfos = referenceEntity.toModificationInfos();
            modificationReferenceInfos.setReferenceInfos(toModificationsInfosOptimized(referencedEntity));
            return modificationReferenceInfos;
        } else {
            ModificationEntity referencedEntity = modificationRepository.findReferencedModificationMetadataByReferenceId(modificationEntity.getId());
            if (referencedEntity == null) {
                throw new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationEntity.getId()));
            }
            ModificationInfos modificationInfos = modificationEntity.toModificationInfos();
            modificationInfos.setMessageType(referencedEntity.getMessageType());
            modificationInfos.setMessageValues(referencedEntity.getMessageValues());
            return modificationInfos;
        }
    }

    private ModificationInfos toModificationsInfosOptimized(ModificationEntity modificationEntity) {
        return toModificationsInfosOptimized(modificationEntity, Set.of(), true);
    }

    private ModificationInfos toModificationsInfosOptimized(ModificationEntity modificationEntity, Set<UUID> modificationsToExclude, boolean rootModification) {
        if (modificationEntity instanceof CompositeModificationEntity compositeEntity) {
            if (rootModification) {
                prefetchCompositeSubTree(compositeEntity);
            }
            return loadCompositeModification(compositeEntity, modificationsToExclude);
        } else if (ModificationType.COMPOSITE_MODIFICATION.name().equals(modificationEntity.getType())) {
            // defensive: a base projection that lost its subclass — metadata-only view, depth unknown
            return loadCompositeModificationMetadata(modificationEntity, null);
        }
        if (modificationEntity instanceof TabularModificationsEntity tabularEntity) {
            return loadTabularModification(tabularEntity);
        }
        if (ModificationType.MODIFICATION_REFERENCE.name().equals(modificationEntity.getType())) {
            return loadModificationReference(modificationEntity);
        }
        return modificationEntity.toModificationInfos();
    }

    private ModificationInfos toModificationMetadataInfos(ModificationEntity modificationEntity, Map<UUID, Integer> depths) {
        if (ModificationType.COMPOSITE_MODIFICATION.name().equals(modificationEntity.getType())) {
            return loadCompositeModificationMetadata(modificationEntity, depths.get(modificationEntity.getId()));
        }
        if (ModificationType.MODIFICATION_REFERENCE.name().equals(modificationEntity.getType())) {
            return loadModificationReference(modificationEntity);
        }
        return modificationEntity.toModificationInfos();
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getActiveModifications(UUID groupUuid, @NonNull Set<UUID> modificationsToExclude) {
        List<ModificationEntity> modificationsEntities = modificationRepository.findAllActiveModificationsByContainerId(groupUuid, emptyIfNull(modificationsToExclude));
        return modificationsEntities.stream().map(m -> toModificationsInfosOptimized(m, modificationsToExclude, true)).toList();
    }

    private List<ModificationInfos> getModificationsInfos(List<UUID> groupUuids, boolean onlyStashed) {
        return groupUuids.stream().flatMap(this::getModificationEntityStream)
                .filter(m -> !onlyStashed || m.getStashed() == onlyStashed)
                .map(this::toModificationsInfosOptimized).toList();
    }

    public List<ModificationInfos> getModificationsInfosToExport(List<UUID> groupUuids, boolean errorOnGroupNotFound) {
        try {
            return groupUuids.stream().flatMap(this::getModificationEntityStream)
                    .filter(modification -> !modification.getStashed())
                    .map(this::toModificationsInfosOptimized).toList();
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND && !errorOnGroupNotFound) {
                return List.of();
            }
            throw e;
        }
    }

    @Transactional(readOnly = true)
    public ModificationInfos getModificationInfo(UUID modificationUuid) {
        return toModificationsInfosOptimized(getModificationEntity(modificationUuid));
    }

    public ModificationEntity getModificationEntity(UUID modificationUuid) {
        return modificationRepository
                .findById(modificationUuid)
                .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, modificationUuid.toString()));
    }

    @Transactional
    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        try {
            ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
            if (!groupEntity.getModifications().isEmpty()) {
                deleteModifications(groupEntity.getModifications().stream().filter(Objects::nonNull).toList());
            }
            // deleting the group deletes its modification_container row (JOINED subtype delete)
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
            Optional<ModificationEntity> optionalModificationWithGroup = modifications.stream().filter(m -> m.getContainerUuid() != null).findFirst();
            if (optionalModificationWithGroup.isPresent()) {
                throw new NetworkModificationException(MODIFICATION_DELETION_ERROR, String.format("%s is owned by group %s",
                        optionalModificationWithGroup.get().getId().toString(), optionalModificationWithGroup.get().getContainerUuid()));
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
        return this.modificationGroupRepository.findById(groupUuid)
                .orElseGet(
                        () -> modificationGroupRepository.save(new ModificationGroupEntity(groupUuid)));
    }

    private Stream<ModificationEntity> getModificationEntityStream(UUID groupUuid) {
        return getModificationGroup(groupUuid).getModifications().stream().filter(Objects::nonNull);
    }

    @Transactional(readOnly = true)
    public Integer getModificationsCount(@NonNull UUID groupUuid, boolean stashed) {
        return modificationRepository.countByContainerAndStashed(groupUuid, stashed);
    }

    private List<ModificationInfos> getModificationsInfosNonTransactional(List<UUID> uuids) {
        // Spring-data findAllById doc says: the order of elements in the result is not guaranteed
        Map<UUID, ModificationEntity> entities = modificationRepository.findAllById(uuids)
                .stream()
                .collect(Collectors.toMap(
                        ModificationEntity::getId,
                        Function.identity()
                ));
        return uuids.stream().map(entities::get).filter(Objects::nonNull).map(this::toModificationsInfosOptimized).toList();
    }

    /**
     * returns the data from all the network modifications contained in the composite modifications sent as parameter
     * but only returns the basic data common to all the modifications form the ModificationInfos, not from the extended classes
     */
    @Transactional(readOnly = true)
    public List<ModificationInfos> getBasicNetworkModificationsFromComposite(@NonNull List<UUID> uuids) {
        List<UUID> networkModificationsUuids = modificationRepository.findAllByContainers(uuids).stream().map(ModificationEntity::getId).toList();
        Map<UUID, ModificationEntity> entitiesById = modificationRepository.findBaseDataByIdIn(networkModificationsUuids).stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity()));
        Map<UUID, Integer> depths = batchCompositeDepths(entitiesById.values());
        return new ArrayList<>(networkModificationsUuids.stream()
                .map(entitiesById::get)
                .filter(Objects::nonNull)
                .map(m -> toModificationMetadataInfos(m, depths))
                .toList());
    }

    @Transactional(readOnly = true)
    public List<UUID> findAllChildrenUuids(@NonNull List<UUID> compositeUuids) {
        return compositeUuids.stream().flatMap(uuid -> modificationRepository.findAllChildrenUuids(uuid).stream()).toList();
    }

    public Map<UUID, Integer> getCompositesMaxDepthMap(@NonNull List<UUID> compositeUuids) {
        if (compositeUuids.isEmpty()) {
            return Map.of();
        }
        return modificationRepository.getCompositesMaxDepth(compositeUuids).stream()
                .collect(Collectors.toMap(c -> UUID.fromString(c.getId()), ModificationRepository.CompositeDepth::getDepth));
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getCompositeModificationsInfos(@NonNull List<UUID> uuids) {
        return getModificationsInfosInsideCompositesNonTransactional(uuids);
    }

    private List<ModificationInfos> getModificationsInfosInsideCompositesNonTransactional(@NonNull List<UUID> compositeUuids) {
        List<ModificationInfos> entities = new ArrayList<>();
        compositeUuids.forEach(uuid -> {
            List<UUID> foundEntities = modificationRepository.findAllByContainer(uuid).stream().map(ModificationEntity::getId).toList();
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
        return getModificationEntityStream(groupUuid).filter(m -> !m.getStashed()).map(this::toModificationsInfosOptimized).toList();
    }

    /**
     * @return elementUuid of the shared modification -> Uuid of the composite containing the reference, null if the modification reference is at the root level
     */
    @Transactional
    public Map<UUID, UUID> getReferences(@NonNull List<UUID> modificationUuids) {
        Map<UUID, UUID> references = new HashMap<>();

        List<ModificationEntity> modificationEntities = this.modificationRepository.findAllByIdIn(modificationUuids);

        // TODO GRD-4785 : for now shared modification are only at the root level and can't be inside composites, so the composite uuid is set to null
        // but when it will be the case a specific function will have to be done in order to fetch the composite containing the modificationReference (if there is one)
        modificationEntities.forEach(modificationEntity -> {
            if (modificationEntity instanceof ModificationReferenceEntity modificationReference) {
                references.putIfAbsent(modificationReference.getReferenceId(), null);
            }
        });

        return references;
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
        List<ModificationEntity> entities = this.modificationRepository.findAllByContainerId(groupId, stashed);
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
    public void updateNetworkModificationMetadata(@NonNull List<UUID> modificationUuids, @NonNull ModificationInfos metadata) {
        for (UUID modificationUuid : modificationUuids) {
            ModificationEntity modificationEntity = this.modificationRepository
                    .findById(modificationUuid)
                    .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, modificationUuid)));
            if (metadata.getDescription() != null) {
                modificationEntity.setDescription(metadata.getDescription());
            }
            if (metadata.getActivated() != null) {
                updateActivated(modificationEntity, metadata.getActivated());
            }
            if (metadata instanceof CompositeModificationInfos compositeMetadata
                    && modificationEntity instanceof CompositeModificationEntity composite
                    && compositeMetadata.getName() != null) {
                compositeModificationRepository.updateCompositeModificationMetadata(composite, compositeMetadata);
            }
        }
    }

    // TODO remove when activation for a sub modification (composite) is implemented : no need optimized load
    private void updateActivated(ModificationEntity entity, boolean activated) {
        entity.setActivated(activated);
        if (entity instanceof CompositeModificationEntity composite) {
            composite.getModifications().forEach(sub -> updateActivated(sub, activated));
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

        // delete tabular modifications/creations
        List<TabularModificationsEntity> tabularModificationsToDelete = modificationEntities.stream().filter(TabularModificationsEntity.class::isInstance).map(
                TabularModificationsEntity.class::cast).toList();
        tabularModificationsToDelete.forEach(m -> m.setContainer(null));
        tabularModificationsToDelete.forEach(this::deleteTabularModification);

        List<CompositeModificationEntity> compositesToDelete = modificationEntities.stream()
                .filter(CompositeModificationEntity.class::isInstance)
                .map(CompositeModificationEntity.class::cast)
                .toList();
        deleteCompositeChildrenSubtree(compositesToDelete);

        List<UUID> uuidsToDelete = modificationEntities.stream()
                .filter(Predicate.not(TabularModificationsEntity.class::isInstance))
                .map(ModificationEntity::getId).toList();
        if (!uuidsToDelete.isEmpty()) {
            modificationApplicationInfosService.deleteAllByModificationIds(uuidsToDelete);
            modificationRepository.deleteAllByIdIn(uuidsToDelete);
            // bulk delete bypasses orphanRemoval; content shares the composite's id, so reap by the same ids.
            // (Or declare ON DELETE CASCADE composite_container.id -> modification_container.id and skip this.)
            List<UUID> compositeIds = compositesToDelete.stream().map(ModificationEntity::getId).toList();
            compositeContainerRepository.deleteAllById(compositeIds);
        }
    }

    private void deleteCompositeChildrenSubtree(List<CompositeModificationEntity> composites) {
        // content id == composite id, so container ids ARE the composite ids
        List<UUID> containerIds = composites.stream().map(ModificationEntity::getId).toList();
        List<ModificationEntity> descendants = new ArrayList<>();
        while (!containerIds.isEmpty()) {
            List<ModificationEntity> children = modificationRepository.findAllByContainers(containerIds);
            descendants.addAll(children);
            containerIds = children.stream()
                    .filter(CompositeModificationEntity.class::isInstance)
                    .map(ModificationEntity::getId)
                    .toList();
        }
        if (!descendants.isEmpty()) {
            deleteModifications(modificationRepository.findAllById(descendants.stream().map(ModificationEntity::getId).toList()));
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
        List<ModificationInfos> modificationInfos = originGroupUuid != null ? getUnstashedModificationsInfosNonTransactional(originGroupUuid) : getModificationsInfosNonTransactional(
                modificationsUuids);
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, modificationInfos);
        // We can't return modificationInfos directly because it wouldn't have the IDs coming from the new saved entities
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    @Transactional
    public List<ModificationInfos> extractModificationsFromCompositesAndSave(@NonNull UUID targetGroupUuid, @NonNull List<UUID> compositesUuids) {
        List<ModificationInfos> modificationInfos = getModificationsInfosInsideCompositesNonTransactional(compositesUuids);
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, modificationInfos);
        // We can't return modificationInfos directly because it wouldn't have the IDs coming from the new saved entities
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    @Transactional
    public List<ModificationInfos> insertCompositeModifications(
            @NonNull UUID targetGroupUuid,
            @NonNull List<CompositeInfos> compositeInfos) {
        List<ModificationInfos> newCompositeModifications = new ArrayList<>();
        for (CompositeInfos compositeToBeInserted : compositeInfos) {
            CompositeModificationInfos compositeModification = (CompositeModificationInfos) getModificationEntity(compositeToBeInserted.id()).toModificationInfos();
            if (compositeModification != null) {
                if (compositeToBeInserted.isShared()) {
                    ModificationReferenceInfos newModificationReference = ModificationReferenceInfos.builder()
                            .referenceId(compositeToBeInserted.id())
                            .referenceType(ModificationReferenceInfos.Type.BASIC)
                            .referenceInfos(compositeModification)
                            .build();
                    newCompositeModifications.add(newModificationReference);
                } else {
                    // apply the new composite name to the corresponding composite modification
                    compositeModification.setName(compositeToBeInserted.name());
                    newCompositeModifications.add(compositeModification);
                }
            } else {
                LOGGER.error("Could not find composite modification with uuid {} to apply its name {}", compositeToBeInserted.id(), compositeToBeInserted.name());
            }
        }
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, newCompositeModifications);
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    private ModificationContainer resolveContainer(ModificationContainerType type, UUID id, boolean createIfMissing) {
        return switch (type) {
            case GROUP -> createIfMissing ? getOrCreateModificationGroup(id) : getModificationGroup(id);
            case COMPOSITE -> compositeContainerRepository.findById(id)
                    .map(ModificationContainer.class::cast)
                    .orElseThrow(() -> new NetworkModificationException(
                            MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND_MESSAGE, id)));
        };
    }

    public ModificationContainerType getContainerType(ModificationEntity m) {
        UUID cid = m.getContainerUuid();
        if (cid == null) {
            return null;
        }
        return getContainerTypeOrCreateGroup(cid, false);
    }

    public ModificationContainerType getContainerTypeOrCreateGroup(UUID id, boolean createGroupIfMissing) {
        ModificationContainerType containerType = modificationContainerRepository.getTypeById(id);
        if (containerType != null) {
            return containerType;
        } else if (createGroupIfMissing) {
            getOrCreateModificationGroup(id);
            return ModificationContainerType.GROUP;
        }
        throw new NetworkModificationException(MODIFICATION_NOT_FOUND,
                String.format("No modification container found for id %s", id));
    }

    @Transactional
    public CompositeModificationEntity assembleNetworkModificationsIntoNewComposite(List<UUID> assembledModificationsUuids) {
        final UUID firstModifUuid = assembledModificationsUuids.getFirst();
        final ModificationEntity firstModificationEntity = getModificationEntity(firstModifUuid);
        final int targetIndex = firstModificationEntity.getModificationsOrder();
        ModificationGroupEntity targetGroup = null;
        CompositeContainerEntity targetComposite = null;
        if (getContainerType(firstModificationEntity) == ModificationContainerType.GROUP) {
            targetGroup = modificationGroupRepository.findById(firstModificationEntity.getContainerUuid()).orElse(null);
        } else {
            targetComposite = compositeContainerRepository.findById(firstModificationEntity.getContainerUuid()).orElse(null);
        }

        List<ModificationEntity> assembledModifications = assembledModificationsUuids.stream()
                .map(modificationRepository::findById).filter(Optional::isPresent).map(Optional::get).toList();

        // 1. clean the origin group, if any
        UUID originContainerId = assembledModifications.stream()
                .filter(mod -> getContainerType(mod) == ModificationContainerType.GROUP)
                .map(ModificationEntity::getContainerUuid).findFirst().orElse(null);
        ModificationGroupEntity originGroup = originContainerId != null
                ? modificationGroupRepository.findById(originContainerId).orElse(null) : null;
        if (originGroup != null) {
            List<ModificationEntity> kept = new ArrayList<>(originGroup.getModifications());
            kept.removeIf(mod -> assembledModificationsUuids.contains(mod.getId()));
            originGroup.setModifications(kept);
        }

        // 2. clean composites whose sub-modifications are assembled away
        for (ModificationEntity assembled : assembledModifications.stream()
                .filter(mod -> getContainerType(mod) == ModificationContainerType.COMPOSITE).toList()) {
            CompositeContainerEntity previousOwner = compositeContainerRepository.findById(assembled.getContainerUuid()).orElse(null);
            if (previousOwner != null) {
                List<ModificationEntity> left = new ArrayList<>(previousOwner.getModifications());
                left.removeIf(mod -> assembledModificationsUuids.contains(mod.getId()));
                previousOwner.setModifications(left);
            }
        }

        CompositeModificationInfos newCompositeInfos = CompositeModificationInfos.builder()
                .modificationsInfos(List.of())
                .name("Composite modification")
                .build();
        CompositeModificationEntity newLeaf = (CompositeModificationEntity) ModificationEntity.fromDTO(newCompositeInfos);
        newLeaf.setModifications(assembledModifications);

        if (targetGroup != null) {
            List<ModificationEntity> mods = new ArrayList<>(targetGroup.getModifications());
            mods.add(targetIndex, newLeaf);
            targetGroup.setModifications(mods);
        } else if (targetComposite != null) {
            List<ModificationEntity> mods = new ArrayList<>(targetComposite.getModifications());
            mods.add(targetIndex, newLeaf);
            targetComposite.setModifications(mods);
        }
        return modificationRepository.save(newLeaf);
    }
}
