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
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.dto.tabular.LimitSetsTabularModificationInfos;
import org.gridsuite.modification.dto.tabular.TabularBaseInfos;
import org.gridsuite.modification.dto.tabular.TabularCreationInfos;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.server.dto.CompositeInfos;
import org.gridsuite.modification.server.dto.ModificationContainerInfos;
import org.gridsuite.modification.server.dto.ModificationMetadata;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.entities.equipment.modification.EquipmentModificationEntity;
import org.gridsuite.modification.server.entities.tabular.TabularModificationsEntity;
import org.gridsuite.modification.server.entities.tabular.TabularPropertyEntity;
import org.gridsuite.modification.server.error.NetworkModificationServerException;
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
import static org.gridsuite.modification.server.error.ModificationBusinessErrorCode.*;
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

    private NetworkModificationServerException getModificationContainerNotFoundException(String containerId, ModificationContainerType containerType) {
        return new NetworkModificationServerException(MODIFICATION_CONTAINER_NOT_FOUND,
            String.format(MODIFICATION_CONTAINER_NOT_FOUND.messageTemplate(), containerId, containerType.name()),
            Map.of("containerId", containerId, "containerType", containerType.name()));
    }

    private NetworkModificationServerException getModificationNotFoundException(String modificationId) {
        return new NetworkModificationServerException(MODIFICATION_NOT_FOUND, String.format(MODIFICATION_NOT_FOUND.messageTemplate(), modificationId), Map.of("modificationId", modificationId));
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
                .orElseThrow(() -> getModificationNotFoundException(compositeUuid.toString()));

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
                .orElseThrow(() -> getModificationNotFoundException(compositeUuid.toString()));
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

    /**
     * This function does a prepass to insure all modifications are contained in the same source container before moving them to the target container
     */

    @Transactional
    // TODO Remove this method and use moveModifications instead, after refactoring the front-end to use the new API.
    // This method is kept for backward compatibility with the old front-end.
    // With the refactoring, the source container will be determined by each modification and moveSubModificationsToGroup has to be deleted
    public List<ModificationInfos> moveModificationsFromGroup(
            @NonNull ModificationContainerInfos sourceContainerInfos,
            @NonNull ModificationContainerInfos targetContainerInfos,
            @NonNull List<UUID> modificationUuids, UUID beforeModificationUuid) {
        AbstractModificationContainerEntity sourceContainer = getContainer(sourceContainerInfos);
        AbstractModificationContainerEntity targetContainer = getContainer(targetContainerInfos);
        moveSubModificationsToGroup(sourceContainer, modificationUuids);
        return moveModificationsNonTransactional(sourceContainer, targetContainer, modificationUuids, beforeModificationUuid)
            .stream().map(this::toModificationsInfosOptimized).toList();
    }

    @Transactional
    public List<ModificationInfos> moveModifications(
            @NonNull ModificationContainerInfos sourceContainerInfos,
            @NonNull ModificationContainerInfos targetContainerInfos,
            @NonNull List<UUID> modificationUuids, UUID beforeModificationUuid) {
        AbstractModificationContainerEntity sourceContainer = getContainer(sourceContainerInfos);
        AbstractModificationContainerEntity targetContainer = getContainer(targetContainerInfos);
        return moveModificationsNonTransactional(sourceContainer, targetContainer, modificationUuids, beforeModificationUuid)
                .stream().map(this::toModificationsInfosOptimized).toList();
    }

    /**
     * During a cut operation some selected modifications may currently be nested inside a composite other than
     * {@code sourceId} (e.g. grouped under an unrelated composite ancestor). Before the requested
     * move runs, promote each of those to be a direct child of {@code sourceId}, so that
     * {@link #moveModificationsNonTransactional} always operates on modifications that are
     * genuinely children of the source container. Composite roots among the selection, and
     * modifications already covered by a selected composite ancestor, are left in place — only
     * their loose descendants need surfacing.
     * TODO To be removed (see moveModificationsFromGroup)
     */
    private void moveSubModificationsToGroup(AbstractModificationContainerEntity sourceContainer, List<UUID> modificationUuids) {
        Set<UUID> selectedCompositeUuids = modificationRepository.findExistingCompositeModificationIds(modificationUuids);

        Set<UUID> childrenOfSelectedComposites = selectedCompositeUuids
            .stream()
            .flatMap(uuid -> modificationRepository.findAllChildrenUuids(uuid).stream())
            .collect(Collectors.toCollection(HashSet::new));
        childrenOfSelectedComposites.removeAll(selectedCompositeUuids);

        List<UUID> subModificationUuids = modificationUuids.stream()
                .filter(uuid -> !childrenOfSelectedComposites.contains(uuid))
                .toList();

        for (UUID uuid : subModificationUuids) {
            UUID parentCompositeUuid = modificationRepository.findCompositeContainerIdByModificationId(uuid);
            if (parentCompositeUuid != null && !parentCompositeUuid.equals(sourceContainer.getId())) {
                moveModificationsNonTransactional(
                        getContainer(new ModificationContainerInfos(parentCompositeUuid, ModificationContainerType.COMPOSITE)),
                        sourceContainer,
                        List.of(uuid), null);
            }
        }
    }

    private List<ModificationEntity> moveModificationsNonTransactional(AbstractModificationContainerEntity sourceContainer, AbstractModificationContainerEntity targetContainer,
                                                                        List<UUID> modificationUuids, UUID beforeModificationUuid) {
        boolean sameContainer = sourceContainer.getId().equals(targetContainer.getId());

        if (sameContainer) {
            return sourceContainer.moveModifications(modificationUuids, beforeModificationUuid);
        }

        List<ModificationEntity> modificationsMoved = sourceContainer.removeModifications(modificationUuids);
        if (modificationsMoved.isEmpty()) {
            return List.of();
        }

        if (sourceContainer.isGroup() && targetContainer.isGroup()) {
            modificationApplicationInfosService.deleteAllByModificationIds(collectAllModificationUuids(modificationsMoved));
        }

        targetContainer.insertModifications(modificationsMoved, beforeModificationUuid);

        return modificationsMoved;
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
        } catch (NetworkModificationServerException e) {
            if (e.getBusinessErrorCode() == MODIFICATION_CONTAINER_NOT_FOUND && !errorOnGroupNotFound) {
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
                .orElseThrow(() -> getModificationNotFoundException(referenceEntity.getReferenceId() + " (referenced modification)"));
            ModificationReferenceInfos modificationReferenceInfos = referenceEntity.toModificationInfos();
            ModificationInfos refInfos = toModificationsInfosOptimized(referencedEntity);

            if (refInfos instanceof CompositeModificationInfos composite && composite.getModificationsInfos() != null) {
                composite.getModificationsInfos().forEach(compositeModificationRepository::generateModificationMessage);
            }
            modificationReferenceInfos.setReferenceInfos(refInfos);
            return modificationReferenceInfos;
        } else {
            ModificationEntity referencedEntity = modificationRepository.findReferencedModificationMetadataByReferenceId(modificationEntity.getId());
            if (referencedEntity == null) {
                throw getModificationNotFoundException(modificationEntity.getId() + " (referenced modification)");
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
        } catch (NetworkModificationServerException e) {
            if (e.getBusinessErrorCode() == MODIFICATION_CONTAINER_NOT_FOUND && !errorOnGroupNotFound) {
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
                .orElseThrow(() -> getModificationNotFoundException(modificationUuid.toString()));
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
        } catch (NetworkModificationServerException e) {
            if (e.getBusinessErrorCode() == MODIFICATION_CONTAINER_NOT_FOUND && !errorOnGroupNotFound) {
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
            modifications = groupEntity.getModifications();
            if (uuids != null) {
                modifications = groupEntity.removeModifications(uuids);
            } else {
                groupEntity.removeAllModifications();
            }
        } else if (uuids != null) {
            modifications = modificationRepository.findAllById(uuids);
            Optional<ModificationEntity> optionalModificationWithGroup = modifications.stream().filter(m -> m.getContainer() != null && m.getContainer().isGroup()).findFirst();
            if (optionalModificationWithGroup.isPresent()) {
                throw new NetworkModificationServerException(MODIFICATION_WITH_GROUP_DELETION_FORBIDDEN,
                    String.format(MODIFICATION_WITH_GROUP_DELETION_FORBIDDEN.messageTemplate(), optionalModificationWithGroup.get().getId(), optionalModificationWithGroup.get().getContainerUuid()),
                    Map.of("modificationId", optionalModificationWithGroup.get().getId(), "groupId", optionalModificationWithGroup.get().getContainerUuid()));
            }
        } else {
            throw new NetworkModificationServerException(MODIFICATION_DELETION_ARGUMENT_ERROR, MODIFICATION_DELETION_ARGUMENT_ERROR.messageTemplate());
        }
        int count = modifications.size();
        deleteModifications(modifications);
        return count;
    }

    private ModificationGroupEntity getModificationGroup(UUID groupUuid) {
        return this.modificationGroupRepository.findById(groupUuid)
            .orElseThrow(() -> getModificationContainerNotFoundException(groupUuid.toString(), ModificationContainerType.GROUP));
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
                    .orElseThrow(() -> getModificationNotFoundException(modificationUuid.toString()));
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
            throw new NetworkModificationServerException(MODIFICATIONS_NOT_FOUND,
                String.format("Some of these modifications %s (to be restored) were not found", modificationUuids),
                Map.of("ids", modificationUuids));
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
                    .orElseThrow(() -> getModificationNotFoundException(modificationUuid.toString()));
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
            List<ModificationEntity> modifications = getModificationGroup(groupUuid).removeAllStashedModifications();
            if (!modifications.isEmpty()) {
                deleteModifications(modifications);
            }
        } catch (NetworkModificationServerException e) {
            if (e.getBusinessErrorCode() == MODIFICATION_CONTAINER_NOT_FOUND && !errorOnGroupNotFound) {
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
        if (composites.isEmpty()) {
            return;
        }
        // content id == composite id, so container ids ARE the composite ids
        List<UUID> containerIds = composites.stream().map(ModificationEntity::getId).toList();
        List<UUID> childrenIds = modificationRepository.findAllByContainers(containerIds).stream()
                .map(ModificationEntity::getId)
                .toList();
        if (!childrenIds.isEmpty()) {
            deleteModifications(modificationRepository.findAllById(childrenIds));
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
                    compositeModification.setDescription(compositeToBeInserted.description());
                    newCompositeModifications.add(compositeModification);
                }
            } else {
                LOGGER.error("Could not find composite modification with uuid {} to apply its name {}", compositeToBeInserted.id(), compositeToBeInserted.name());
            }
        }
        List<ModificationEntity> newEntities = saveModificationInfosNonTransactional(targetGroupUuid, newCompositeModifications);
        return newEntities.stream().map(ModificationEntity::toModificationInfos).toList();
    }

    /**
     * Takes a composite modification out of its group so that it can be stored as an element in the directory server,
     * and puts a reference to it at the very same place in the group.
     * @param groupUuid group owning the composite modification
     * @param modificationUuid uuid of the composite modification to share
     * @param name name given to the shared composite modification, null to keep the current one
     * @return the uuid of the extracted composite modification, now standalone
     */
    @Transactional
    public UUID extractCompositeModificationToShare(@NonNull UUID groupUuid, @NonNull UUID modificationUuid, String name) {
        ModificationGroupEntity groupEntity = getModificationGroup(groupUuid);
        ModificationEntity modificationEntity = getModificationEntity(modificationUuid);
        if (!(modificationEntity instanceof CompositeModificationEntity compositeEntity)) {
            String expectedType = ModificationType.COMPOSITE_MODIFICATION.name();
            throw new NetworkModificationServerException(MODIFICATION_BAD_TYPE,
                String.format(MODIFICATION_BAD_TYPE.messageTemplate(), modificationUuid, modificationEntity.getType(), expectedType),
                Map.of("modificationId", modificationUuid.toString(), "modificationType", modificationEntity.getType(), "expectedModificationType", expectedType));
        }
        if (!groupUuid.equals(modificationEntity.getContainerUuid())) {
            throw new NetworkModificationServerException(MODIFICATION_NOT_FOUND,
                String.format("Modification %s is not owned by group %s", modificationUuid, groupUuid),
                Map.of("modificationId", modificationUuid + " (group = " + groupUuid + ")"));
        }

        ModificationReferenceInfos referenceInfos = ModificationReferenceInfos.builder()
            .referenceId(modificationUuid)
            .referenceType(ModificationReferenceInfos.Type.BASIC)
            .referenceInfos(loadCompositeModificationMetadata(compositeEntity, null))
            .build();
        ModificationEntity referenceEntity = ModificationEntity.fromDTO(referenceInfos);

        // the reference takes the place - and the order - of the shared composite modification
        groupEntity.addModification(referenceEntity, compositeEntity.getModificationsOrder());
        groupEntity.removeModifications(List.of(modificationUuid));
        compositeEntity.setContainer(null);
        compositeEntity.setModificationsOrder(0);
        if (name != null) {
            compositeModificationRepository.renameCompositeModification(compositeEntity, name);
        }

        modificationRepository.save(referenceEntity);
        modificationRepository.save(compositeEntity);
        return modificationUuid;
    }

    private AbstractModificationContainerEntity getContainer(ModificationContainerInfos containerInfos) {
        AbstractModificationContainerEntity containerEntity = modificationContainerRepository.findById(containerInfos.id()).orElseGet(() -> {
            if (ModificationContainerType.GROUP.equals(containerInfos.type())) {
                return modificationGroupRepository.save(new ModificationGroupEntity(containerInfos.id()));
            } else {
                throw getModificationContainerNotFoundException(containerInfos.id().toString(), containerInfos.type());
            }
        });
        if (!containerInfos.type().name().equals(containerEntity.getType())) {
            throw new NetworkModificationServerException(MODIFICATION_CONTAINER_BAD_TYPE,
                String.format(MODIFICATION_CONTAINER_BAD_TYPE.messageTemplate(), containerInfos.id(), containerEntity.getType(), containerInfos.type().name()),
                Map.of("containerId", containerInfos.id(), "containerType", containerEntity.getType(), "expectedContainerType", containerInfos.type().name()));
        }
        return containerEntity;
    }

    public ModificationContainerType getContainerType(ModificationEntity m) {
        ModificationContainerType containerType = modificationContainerRepository.getTypeById(m.getContainerUuid());
        if (containerType == null) {
            throw new NetworkModificationServerException(MODIFICATION_CONTAINER_TYPE_NOT_FOUND,
                String.format(MODIFICATION_CONTAINER_TYPE_NOT_FOUND.messageTemplate(), m.getId()),
                Map.of("containerId", m.getId()));
        }
        return containerType;
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
            List<ModificationEntity> mods = targetGroup.getNonStashedModifications();
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
