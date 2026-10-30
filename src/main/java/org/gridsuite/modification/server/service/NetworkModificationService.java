/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import co.elastic.clients.elasticsearch._types.query_dsl.BoolQuery;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import lombok.NonNull;
import org.apache.commons.collections4.SetUtils;
import org.apache.commons.lang3.StringUtils;
import org.gridsuite.modification.context.ModificationContext;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.modifications.AbstractModification;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosService;
import org.gridsuite.modification.server.entities.*;
import org.gridsuite.modification.server.error.NetworkModificationServerException;
import org.gridsuite.modification.server.modifications.ModificationTypeWithPreloadingStrategy;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.client.elc.Queries;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.util.Pair;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.HttpClientErrorException;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.error.ModificationBusinessErrorCode.*;
import static org.gridsuite.modification.server.modifications.AsyncUtils.scheduleApplyModifications;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Service
public class NetworkModificationService {
    private final NetworkStoreService networkStoreService;

    private final NetworkModificationRepository networkModificationRepository;

    private final NetworkModificationApplicator modificationApplicator;

    private final EquipmentInfosService equipmentInfosService;

    private final NotificationService notificationService;

    private final ObjectMapper objectMapper;

    private final ElasticsearchOperations elasticsearchOperations;

    private final ModificationApplicationInfosService applicationInfosService;

    private final DirectoryService directoryService;

    private final ModificationContextFactory modificationContextFactory;

    private final ModificationFilterNameService modificationFilterNameService;

    static final String NETWORK_UUID = "networkUuid.keyword";
    static final String CREATED_EQUIPMENT_IDS = "createdEquipmentIds.fullascii";
    static final String MODIFIED_EQUIPMENT_IDS = "modifiedEquipmentIds.fullascii";
    static final String DELETED_EQUIPMENT_IDS = "deletedEquipmentIds.fullascii";
    private final ModificationRepository modificationRepository;
    private static final int PAGE_MAX_SIZE = 500;

    public NetworkModificationService(NetworkStoreService networkStoreService,
                                      NetworkModificationRepository networkModificationRepository,
                                      EquipmentInfosService equipmentInfosService,
                                      NotificationService notificationService,
                                      NetworkModificationApplicator applicationService,
                                      ObjectMapper objectMapper,
                                      ModificationApplicationInfosService applicationInfosService,
                                      ElasticsearchOperations elasticsearchOperations,
                                      ModificationRepository modificationRepository,
                                      DirectoryService directoryService,
                                      ModificationContextFactory modificationContextFactory,
                                      ModificationFilterNameService modificationFilterNameService) {
        this.networkStoreService = networkStoreService;
        this.networkModificationRepository = networkModificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.notificationService = notificationService;
        this.modificationApplicator = applicationService;
        this.objectMapper = objectMapper;
        this.applicationInfosService = applicationInfosService;
        this.elasticsearchOperations = elasticsearchOperations;
        this.modificationRepository = modificationRepository;
        this.directoryService = directoryService;
        this.modificationContextFactory = modificationContextFactory;
        this.modificationFilterNameService = modificationFilterNameService;
    }

    public List<UUID> getModificationGroups() {
        return networkModificationRepository.getModificationGroupsUuids();
    }

    public List<ModificationInfos> getNetworkModifications(UUID containerUuid, boolean onlyMetadata, StashedFilter stashedFilter) {
        List<ModificationInfos> modifications = networkModificationRepository.getModifications(containerUuid, onlyMetadata, stashedFilter);
        modificationFilterNameService.addFilterNames(modifications);
        return modifications;
    }

    @Transactional(readOnly = true)
    public NetworkModificationExportInfos getNetworkModificationsInfosToExport(UUID containerUuid) {
        List<ModificationInfos> allModifications = networkModificationRepository.getModifications(containerUuid, false, StashedFilter.UNSTASHED);
        List<ModificationInfos> exportable = new ArrayList<>();
        List<NetworkModificationExportInfos.UnexportedModification> unexported = new ArrayList<>();
        for (ModificationInfos modification : allModifications) {
            if (modification instanceof EquipmentModificationInfos) {
                exportable.add(modification);
            } else {
                unexported.add(
                        new NetworkModificationExportInfos.UnexportedModification(modification.getUuid(), modification.getType())
                );
            }
        }
        return new NetworkModificationExportInfos(exportable, unexported);
    }

    public List<ModificationEntity> getModificationsByUuids(List<UUID> modificationUuids) {
        return modificationRepository.findAllByIdIn(modificationUuids);
    }

    /**
     * checks that all the modificationUuids are part of the container, otherwise throws an error
     */
    @Transactional(readOnly = true)
    public void verifyModifications(UUID containerUuid, Set<UUID> modificationUuids) {
        networkModificationRepository.assertContainerExists(containerUuid);
        HashSet<UUID> childrenUuids = new HashSet<>(modificationRepository.findAllChildrenMetadata(containerUuid, false)
                .stream().map(ModificationRepository.ChildModificationMetadata::getId).toList());

        if (!childrenUuids.containsAll(modificationUuids)) {
            throw new NetworkModificationServerException(MODIFICATIONS_NOT_FOUND,
                String.format("Some of these modifications %s (to be verified) were not found", modificationUuids),
                Map.of("ids", modificationUuids));
        }
    }

    @Transactional(readOnly = true)
    public List<UUID> findAllChildrenUuids(List<UUID> compositeModificationUuids) {
        return networkModificationRepository.findAllChildrenUuids(compositeModificationUuids);
    }

    public ModificationInfos getNetworkModification(UUID networkModificationUuid) {
        ModificationInfos modification = networkModificationRepository.getModificationInfo(networkModificationUuid);
        modificationFilterNameService.addFilterNames(modification);
        return modification;
    }

    public AbstractModification getStandaloneNetworkModification(UUID networkModificationUuid) {
        return networkModificationRepository.getStandaloneModificationInfos(networkModificationUuid)
                .toModification(modificationContextFactory.create());
    }

    public Map<UUID, AbstractModification> getStandaloneNetworkModifications(List<UUID> networkModificationUuids, boolean errorOnModificationNotFound) {
        List<ModificationInfos> modificationsInfos = networkModificationRepository.getStandaloneModificationsInfos(networkModificationUuids, errorOnModificationNotFound);
        ModificationContext modificationContext = modificationContextFactory.create();
        return modificationsInfos.stream()
                .collect(Collectors.toMap(ModificationInfos::getUuid, modificationInfos -> modificationInfos.toModification(modificationContext)));
    }

    public Integer getNetworkModificationsCount(UUID containerUuid, boolean stashed) {
        return networkModificationRepository.getModificationsCount(containerUuid, stashed);
    }

    public void deleteModificationGroups(List<UUID> groupUuids, @NonNull String userId) {
        Map<UUID, List<ModificationReferenceData>> deletedReferences = networkModificationRepository.deleteModificationGroups(groupUuids);
        deleteSharingLinks(deletedReferences, userId);
    }

    @Transactional
    public void deleteIndexedModificationGroups(List<UUID> groupUuids, UUID networkUuid) {
        applicationInfosService.deleteAllByGroupUuidsAndNetworkUuid(groupUuids, networkUuid);
    }

    public NetworkInfos getNetworkInfos(UUID networkUuid, String variantId, PreloadingStrategy preloadingStrategy) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid, preloadingStrategy);
        } catch (PowsyblException e) {
            throw new NetworkModificationServerException(NETWORK_NOT_FOUND, String.format(NETWORK_NOT_FOUND.messageTemplate(), networkUuid), Map.of("networkId", networkUuid));
        }
        boolean isVariantPresent = true;
        if (variantId != null) {
            if (network.getVariantManager().getVariantIds().stream().anyMatch(id -> id.equals(variantId))) {
                network.getVariantManager().setWorkingVariant(variantId);
            } else {
                isVariantPresent = false;
            }
        }
        return new NetworkInfos(network, networkUuid, isVariantPresent);
    }

    public void checkModificationsPermission(@NonNull List<UUID> modificationUuids, @NonNull String userId) {
        Set<UUID> uuids = new LinkedHashSet<>(
                networkModificationRepository.getAllReferencedModificationAncestorsUuids(modificationUuids)
        );
        modificationUuids.stream()
                .filter(networkModificationRepository::isModificationReferenced)
                .forEach(uuids::add);

        // add also referenced modifications
        uuids.addAll(modificationRepository.findReferencedModificationIds(modificationUuids));

        if (!uuids.isEmpty()) {
            try {
                directoryService.checkPermission(new ArrayList<>(uuids), userId, PermissionType.WRITE);
            } catch (HttpClientErrorException.Forbidden _) {
                throw new NetworkModificationServerException(MODIFICATIONS_CONTAINS_WRITE_FORBIDDEN_SHARED);
            }
        }
    }

    @Transactional
    public void updateNetworkModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos, @NonNull String userId) {
        checkModificationsPermission(List.of(modificationUuid), userId);
        networkModificationRepository.updateModification(modificationUuid, modificationInfos);
        emitReferencedAncestorsUpdated(List.of(modificationUuid), userId);
    }

    @Transactional
    public void updateNetworkModificationActivation(@NonNull List<UUID> modificationUuids, @NonNull ModificationInfos metadata, @NonNull String userId) {
        // TODO replace metadata by boolean in the endpoint
        List<UUID> parentComposites = findModificationParentCompositesNonTransactional(modificationUuids).values().stream().toList();
        checkModificationsPermission(parentComposites, userId);
        networkModificationRepository.updateModificationActivation(modificationUuids, metadata);
        emitReferencedAncestorsUpdated(modificationUuids, userId);
    }

    /** Notify directory-server once per referenced ancestor composite of the given modifications */
    private void emitReferencedAncestorsUpdated(Collection<UUID> modificationUuids, String userId) {
        emitReferencedModificationsUpdated(networkModificationRepository.getAllReferencedModificationAncestorsUuids(modificationUuids), userId);
    }

    /** Notify directory-server once per referenced ancestor composite */
    private void emitReferencedModificationsUpdated(List<UUID> referencedModificationUuids, String userId) {
        referencedModificationUuids.forEach(uuid -> notificationService.emitElementUpdated(uuid, userId));
    }

    @Transactional
    public void updateRootNetworkApplicability(@NonNull List<UUID> modificationUuids, @NonNull String rootNetworkTag, boolean applicable, String userId) {
        assertRootNetworkTagFits(rootNetworkTag);
        checkModificationsPermission(modificationUuids, userId);
        networkModificationRepository.updateRootNetworkApplicability(modificationUuids, rootNetworkTag, applicable);

        modificationRepository.findReferencedModificationIds(modificationUuids)
                .forEach(referencedUuid -> notificationService.emitElementUpdated(referencedUuid, userId));
        emitReferencedAncestorsUpdated(modificationUuids, userId);
    }

    @Transactional
    public void addRootNetworkTag(@NonNull List<UUID> groupUuids, @NonNull String newTag) {
        assertRootNetworkTagFits(newTag);
        networkModificationRepository.addRootNetworkTag(groupUuids, newTag);
    }

    /**
     * Renames the root network tag of the modifications of the given groups. The shared modifications the user cannot
     * write on are left out of the renaming: their applicabilities, and the ones of their content, are left untouched.
     */
    @Transactional
    public void renameRootNetworkTag(@NonNull List<UUID> groupUuids, @NonNull String oldTag, @NonNull String newTag, @NonNull String userId) {
        assertRootNetworkTagFits(newTag);
        List<UUID> ownedUuids = networkModificationRepository.getContainedModificationUuids(groupUuids);
        Set<UUID> sharedUuids = networkModificationRepository.getSharedModificationUuids(ownedUuids);
        Set<UUID> writableSharedUuids = SetUtils.difference(sharedUuids, getNotWritableUuids(sharedUuids, userId));
        networkModificationRepository.renameRootNetworkTag(ownedUuids, writableSharedUuids, oldTag, newTag);
    }

    private Set<UUID> getNotWritableUuids(Set<UUID> elementUuids, String userId) {
        if (elementUuids.isEmpty()) {
            return Set.of();
        }
        // directory server leaves out the elements without permission, and the ones it does not know: they are not writable
        Map<UUID, PermissionType> permissions = directoryService.getElementsPermissions(elementUuids, userId);
        return elementUuids.stream()
            .filter(uuid -> permissions.get(uuid) == null || !permissions.get(uuid).grants(PermissionType.WRITE))
            .collect(Collectors.toSet());
    }

    /**
     * Asserts that the shared modifications the user cannot write on, among the given ones and the ones the given
     * containers or modifications refer to, are associated with the given root network tags: the user cannot add the
     * missing ones. The containers themselves are not checked, only their content: a composite passed as a container,
     * unlike a shared one, is left out.
     */
    @Transactional(readOnly = true)
    public void assertRootNetworkTagsAllowed(@NonNull List<UUID> containerUuids, @NonNull List<UUID> modificationUuids,
                                             @NonNull List<UUID> sharedUuids, @NonNull List<String> rootNetworkTags, @NonNull String userId) {
        if (rootNetworkTags.isEmpty()) {
            return;
        }
        Set<UUID> notWritableUuids = getNotWritableUuids(
                networkModificationRepository.getSharedModificationUuids(containerUuids, modificationUuids, sharedUuids), userId);
        if (notWritableUuids.isEmpty()) {
            return;
        }
        List<String> allowedTags = networkModificationRepository.getCommonRootNetworkTags(notWritableUuids);
        List<String> forbiddenTags = rootNetworkTags.stream().filter(tag -> !allowedTags.contains(tag)).distinct().toList();
        if (!forbiddenTags.isEmpty()) {
            throw new NetworkModificationServerException(ROOT_NETWORK_TAGS_FORBIDDEN_BY_SHARED,
                    String.format(ROOT_NETWORK_TAGS_FORBIDDEN_BY_SHARED.messageTemplate(), forbiddenTags, notWritableUuids, allowedTags),
                    Map.of("allowedTags", String.join(", ", allowedTags), "allowedTagsCount", allowedTags.size()));
        }
    }

    @Transactional
    public void deleteRootNetworkTags(@NonNull List<UUID> groupUuids, @NonNull List<String> rootNetworkTags) {
        networkModificationRepository.deleteRootNetworkTags(groupUuids, rootNetworkTags);
    }

    private static void assertRootNetworkTagFits(String rootNetworkTag) {
        if (rootNetworkTag.length() > ModificationEntity.ROOT_NETWORK_TAG_MAX_LENGTH) {
            throw new NetworkModificationServerException(ROOT_NETWORK_TAG_TOO_LONG,
                    String.format(ROOT_NETWORK_TAG_TOO_LONG.messageTemplate(), ModificationEntity.ROOT_NETWORK_TAG_MAX_LENGTH),
                    Map.of("maxLength", ModificationEntity.ROOT_NETWORK_TAG_MAX_LENGTH));
        }
    }

    @Transactional(readOnly = true)
    public List<ModificationReferenceData> getModificationReferences(@NonNull UUID containerUuid) {
        return getModificationReferencesNonTransactional(containerUuid);
    }

    @Transactional(readOnly = true)
    public void createSharingLinks(@NonNull UUID groupUuid, boolean recreate, @NonNull String userId) {
        List<ModificationReferenceData> modificationReferencesData = getModificationReferencesNonTransactional(groupUuid);
        notificationService.emitSharingLinksChanged(recreate ? SharingLinkAction.RECREATE : SharingLinkAction.CREATE, groupUuid, modificationReferencesData, userId);
    }

    @Transactional
    public List<ModificationReferenceData> getModificationsReferences(@NonNull List<UUID> modificationUuids, boolean fetchSubModifications) {
        return this.getModificationsReferencesNonTransactional(modificationUuids, fetchSubModifications);
    }

    public List<ModificationReferenceData> getModificationsReferencesNonTransactional(@NonNull List<UUID> modificationUuids, boolean fetchSubModifications) {
        return networkModificationRepository.getModificationsReferences(modificationUuids, fetchSubModifications);
    }

    private List<ModificationReferenceData> getModificationReferencesNonTransactional(@NonNull UUID containerUuid) {
        List<UUID> allModificationUuids = modificationRepository.findAllDescendantModificationIdsByContainerIds(List.of(containerUuid));
        return networkModificationRepository.getModificationsReferences(allModificationUuids, false);
    }

    /**
     * @return the composite holding each of the given modifications, keyed by modification id; a modification
     * sitting directly under a group is absent from the map
     */
    @Transactional(readOnly = true)
    public Map<UUID, UUID> findModificationParentComposites(@NonNull List<UUID> modificationUuids) {
        return findModificationParentCompositesNonTransactional(modificationUuids);
    }

    public Map<UUID, UUID> findModificationParentCompositesNonTransactional(@NonNull List<UUID> modificationUuids) {
        if (modificationUuids.isEmpty()) {
            return Map.of();
        }
        return modificationRepository.findCompositeContainersByModificationIdIn(modificationUuids).stream()
                .collect(Collectors.toMap(c -> UUID.fromString(c.getId()), c -> UUID.fromString(c.getContainerId())));
    }

    @Transactional(readOnly = true)
    public boolean hasModificationReferences(@NonNull List<UUID> containerUuids) {
        return !containerUuids.isEmpty() && modificationRepository.existsReferenceInContainersSubtrees(containerUuids);
    }

    @Transactional(readOnly = true)
    public List<UUID> getContainersWithModificationReferences(@NonNull List<UUID> containerUuids) {
        return containerUuids.isEmpty() ? List.of() : modificationRepository.findContainersWithReferenceInSubtrees(containerUuids);
    }

    @Transactional(readOnly = true)
    public SharedModificationsWriteAccess getReferencedModificationsWriteAccess(@NonNull List<UUID> containerUuids, @NonNull String userId) {
        Set<UUID> sharedUuids = networkModificationRepository.getReferencedModificationUuids(containerUuids);
        if (sharedUuids.isEmpty()) {
            return SharedModificationsWriteAccess.NO_SHARED_MODIFICATION;
        }
        int notWritableCount = getNotWritableUuids(sharedUuids, userId).size();
        if (notWritableCount == 0) {
            return SharedModificationsWriteAccess.ALL_WRITABLE;
        }
        if (notWritableCount == sharedUuids.size()) {
            return SharedModificationsWriteAccess.NONE_WRITABLE;
        }
        return SharedModificationsWriteAccess.SOME_WRITABLE;
    }

    public void assertReferencedModificationsAreWritable(@NonNull List<UUID> containerUuids, @NonNull String userId) {
        Set<UUID> sharedModificationUuids = networkModificationRepository.getReferencedModificationUuids(containerUuids);
        if (sharedModificationUuids.isEmpty()) {
            return;
        }
        try {
            directoryService.checkPermission(sharedModificationUuids, userId, PermissionType.WRITE);
        } catch (HttpClientErrorException.Forbidden _) {
            throw new NetworkModificationServerException(MODIFICATIONS_CONTAINS_WRITE_FORBIDDEN_SHARED,
                    String.format(MODIFICATIONS_CONTAINS_WRITE_FORBIDDEN_SHARED.messageTemplate(), sharedModificationUuids),
                    Map.of("sharedModificationUuids", sharedModificationUuids));
        }
    }

    public void stashNetworkModifications(UUID groupUuid, @NonNull List<UUID> modificationUuids, @NonNull String userId) {

        //Check permission only on parent composites
        List<UUID> parentComposites = findModificationParentCompositesNonTransactional(modificationUuids).values().stream().toList();
        checkModificationsPermission(parentComposites, userId);

        // Collect referenced ancestor composites before stashing, since stashed modifications are moved out of their composite
        List<UUID> referencedAncestorUuids = networkModificationRepository.getAllReferencedModificationAncestorsUuids(modificationUuids);

        List<ModificationReferenceData> deletedReferences = networkModificationRepository.stashNetworkModifications(groupUuid, modificationUuids);
        deleteSharingLinks(groupUuid, deletedReferences, userId);

        emitReferencedModificationsUpdated(referencedAncestorUuids, userId);
    }

    @Transactional
    public void reorderNetworkModifications(UUID groupId, Boolean stashed) {
        networkModificationRepository.reorderNetworkModifications(groupId, stashed);
    }

    @Transactional
    public void restoreNetworkModifications(UUID groupUuid, @NonNull List<UUID> modificationUuids) {
        networkModificationRepository.restoreNetworkModifications(modificationUuids,
            networkModificationRepository.getModificationsCount(groupUuid, false));
    }

    public CompletableFuture<NetworkModificationsResult> createNetworkModification(@NonNull UUID groupUuid, @NonNull ModificationInfos modificationInfo,
            @NonNull List<ModificationApplicationContext> applicationContexts) {
        List<ModificationInfos> modifications = networkModificationRepository.saveModificationInfos(groupUuid, List.of(modificationInfo),
            rootNetworkTagsOf(applicationContexts));
        List<UUID> ids = modifications.stream().map(ModificationInfos::getUuid).toList();
        return applyModifications(groupUuid, modifications, applicationContexts).thenApply(results ->
            new NetworkModificationsResult(ids, results));
    }

    public void checkNetworkModification(@NonNull ModificationInfos modificationInfo) {
        try {
            modificationInfo.check();
        } catch (NetworkModificationException e) {
            throw new NetworkModificationServerException(MODIFICATION_INFOS_ERROR,
                String.format(MODIFICATION_INFOS_ERROR.messageTemplate(), e.getMessage()),
                Map.of("errorMessage", e.getMessage()));
        }
    }

    private static List<String> rootNetworkTagsOf(List<ModificationApplicationContext> applicationContexts) {
        return applicationContexts.stream().map(ModificationApplicationContext::rootNetworkTag).filter(Objects::nonNull).distinct().toList();
    }

    /**
     * Apply modifications on several networks
     */
    private CompletableFuture<List<Optional<NetworkModificationResult>>> applyModifications(UUID groupUuid, List<ModificationInfos> modifications,
            List<ModificationApplicationContext> applicationContexts) {
        // Do we want to do these all in parallel (CompletableFuture.allOf) or sequentially (like in Flux.concatMap) or something in between ?
        // sequentially like before for now
        return scheduleApplyModifications(
            modificationApplicationContext ->
                applyModifications(
                    modificationApplicationContext.networkUuid(),
                    modificationApplicationContext.variantId(),
                    new ModificationApplicationGroup(groupUuid,
                        modifications,
                        new ReportInfos(modificationApplicationContext.reportUuid(), modificationApplicationContext.reporterId()),
                        modificationApplicationContext.rootNetworkTag()
                    )
                ),
            applicationContexts
        );
    }

    public Network cloneNetworkVariant(UUID networkUuid,
                                       String originVariantId,
                                       String destinationVariantId,
                                       PreloadingStrategy preloadingStrategy) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid, preloadingStrategy);
            network.addListener(new NetworkVariantsListener(network, networkUuid, equipmentInfosService));
        } catch (PowsyblException e) {
            throw new NetworkModificationServerException(NETWORK_NOT_FOUND, NETWORK_NOT_FOUND.messageTemplate(), Map.of("networkId", networkUuid));
        }
        String startingVariant = StringUtils.isBlank(originVariantId) ? VariantManagerConstants.INITIAL_VARIANT_ID : originVariantId;
        try {
            network.getVariantManager().cloneVariant(startingVariant, destinationVariantId, true);  // cloning variant
            network.getVariantManager().setWorkingVariant(destinationVariantId);  // set current variant to destination variant
        } catch (PowsyblException e) {
            throw new NetworkModificationServerException(VARIANT_NOT_FOUND,
                String.format(VARIANT_NOT_FOUND.messageTemplate(), destinationVariantId, networkUuid),
                Map.of("variantId", destinationVariantId, "networkId", networkUuid));
        }
        return network;
    }

    public NetworkModificationResult buildVariant(@NonNull UUID networkUuid, @NonNull BuildInfos buildInfos) {
        // Apply all modifications belonging to the modification groups uuids in buildInfos
        List<ModificationApplicationGroup> modificationGroupsInfos = new ArrayList<>();
        Streams.forEachPair(buildInfos.getModificationGroupUuids().stream(), buildInfos.getReportsInfos().stream(),
            (groupUuid, reportInfos) -> {
                // a group that does not exist has no modifications
                List<ModificationInfos> modifications = networkModificationRepository.getActiveModifications(groupUuid, buildInfos.getRootNetworkTag());
                modificationGroupsInfos.add(new ModificationApplicationGroup(groupUuid, modifications, reportInfos, buildInfos.getRootNetworkTag()));

            }
        );

        PreloadingStrategy preloadingStrategy = modificationGroupsInfos.stream().map(ModificationApplicationGroup::modifications)
            .flatMap(Collection::stream)
            .map(ModificationInfos::getType)
            .map(ModificationTypeWithPreloadingStrategy::fromModificationType)
            .reduce(ModificationTypeWithPreloadingStrategy::maxStrategy)
            .map(ModificationTypeWithPreloadingStrategy::getStrategy)
            .orElse(PreloadingStrategy.NONE);

        Network network = cloneNetworkVariant(networkUuid, buildInfos.getOriginVariantId(), buildInfos.getDestinationVariantId(), preloadingStrategy);
        NetworkInfos networkInfos = new NetworkInfos(network, networkUuid, true);

        return modificationApplicator.applyModifications(modificationGroupsInfos, networkInfos);
    }

    public void buildVariantRequest(UUID networkUuid, BuildInfos buildInfos, String receiver, WorkflowType workflowType, String workflowInfos) {
        notificationService.emitBuildMessage(new BuildExecContext(networkUuid, buildInfos, receiver, workflowType, workflowInfos).toMessage(objectMapper));
    }

    public void stopBuildRequest(String receiver) {
        notificationService.emitCancelBuildMessage(receiver);
    }

    public void deleteNetworkModifications(@NonNull List<UUID> modificationsUuids, @NonNull String userId) {
        // the modifications may sit in several groups, read before their deletion
        Map<UUID, UUID> groupUuidByReference = getGroupUuidsForModificationReferences(modificationsUuids);
        List<ModificationReferenceData> deletedReferences = networkModificationRepository.deleteModifications(modificationsUuids);
        deleteSharingLinks(groupUuidByReference, deletedReferences, userId);
    }

    /**
     * @return the group holding each of the modification references among the given modifications and their content,
     * keyed by reference id; a reference that doesn't belong to a group (e.g. inside a shared composite) is absent from the map
     */
    private Map<UUID, UUID> getGroupUuidsForModificationReferences(List<UUID> modificationUuids) {
        Map<UUID, UUID> groupByReference = new HashMap<>();
        getModificationsReferencesNonTransactional(modificationUuids, true).forEach(reference -> {
            UUID groupUuid = modificationRepository.findGroupIdByModificationId(reference.modificationUuid());
            if (groupUuid != null) {
                groupByReference.put(reference.modificationUuid(), groupUuid);
            }
        });
        return groupByReference;
    }

    private void deleteSharingLinks(Map<UUID, UUID> groupUuidByReference, List<ModificationReferenceData> modificationReferences, String userId) {
        modificationReferences.stream()
                .filter(reference -> groupUuidByReference.containsKey(reference.modificationUuid()))
                .collect(Collectors.groupingBy(reference -> groupUuidByReference.get(reference.modificationUuid())))
                .forEach((groupUuid, references) -> deleteSharingLinks(groupUuid, references, userId));
    }

    private void deleteSharingLinks(Map<UUID, List<ModificationReferenceData>> modificationReferencesByGroup, String userId) {
        modificationReferencesByGroup.forEach((groupUuid, references) -> deleteSharingLinks(groupUuid, references, userId));
    }

    /**
     * Unlink the deleted modification references from their shared modifications
     */
    private void deleteSharingLinks(UUID groupUuid, List<ModificationReferenceData> modificationReferences, String userId) {
        notificationService.emitSharingLinksChanged(SharingLinkAction.DELETE, groupUuid, modificationReferences, userId);
    }

    public CompletableFuture<NetworkModificationsResult> moveModifications(
            @NonNull UUID originGroupUuid,
            @NonNull UUID targetGroupUuid,
            @NonNull List<ModificationMoveInfos> moveInfos,
            @NonNull List<ModificationApplicationContext> applicationContexts,
            boolean canApply,
            @NonNull String userId) {

        // check if we have permission to move the modifications on source and target groups
        List<UUID> parentComposites = findModificationParentCompositesNonTransactional(moveInfos.stream().map(ModificationMoveInfos::modificationUuid).toList())
                .values().stream().toList();

        List<UUID> uuids = Stream.concat(
                parentComposites.stream(),
                moveInfos.stream().map(ModificationMoveInfos::targetCompositeUuid).filter(Objects::nonNull)
        ).distinct().toList();
        checkModificationsPermission(uuids, userId);

        // Collect referenced ancestor composites before moving, since moved modifications may leave their referenced composite
        Set<UUID> referencedAncestorUuids = new HashSet<>(networkModificationRepository.getAllReferencedModificationAncestorsUuids(
                moveInfos.stream().map(ModificationMoveInfos::modificationUuid).toList()));
        List<ModificationInfos> allMoved = new ArrayList<>();
        // one transaction per move, through the repository proxy
        moveInfos.forEach(m -> allMoved.addAll(networkModificationRepository.moveModifications(
                toContainerInfos(originGroupUuid, m.sourceCompositeUuid()), toContainerInfos(targetGroupUuid, m.targetCompositeUuid()),
                List.of(m.modificationUuid()), m.insertBeforeUuid())));
        List<UUID> movedUuids = allMoved.stream().map(ModificationInfos::getUuid).toList();
        // then the referenced ancestor composites the moved modifications entered
        referencedAncestorUuids.addAll(networkModificationRepository.getAllReferencedModificationAncestorsUuids(movedUuids));
        emitReferencedModificationsUpdated(List.copyOf(referencedAncestorUuids), userId);

        // only modifications entering the target group need to be applied
        if (!canApply || allMoved.isEmpty() || originGroupUuid.equals(targetGroupUuid)) {
            return CompletableFuture.completedFuture(new NetworkModificationsResult(movedUuids, List.of()));
        }

        return applyModifications(targetGroupUuid, allMoved, applicationContexts)
                .thenApply(r -> new NetworkModificationsResult(movedUuids, r));
    }

    /** A null composite designates the group itself */
    private static ModificationContainerInfos toContainerInfos(UUID groupUuid, UUID compositeUuid) {
        return compositeUuid != null
                ? new ModificationContainerInfos(compositeUuid, ModificationContainerType.COMPOSITE)
                : new ModificationContainerInfos(groupUuid, ModificationContainerType.GROUP);
    }

    public void duplicateGroup(@NonNull UUID sourceGroupUuid, @NonNull UUID targetGroupUuid, @NonNull List<String> rootNetworkTags) {
        // the copies may come from another study, using other root network tags
        networkModificationRepository.duplicateUnstashedModifications(sourceGroupUuid, targetGroupUuid, rootNetworkTags);
    }

    private CompletableFuture<Optional<NetworkModificationResult>> applyModifications(UUID networkUuid, String variantId, ModificationApplicationGroup modificationGroupInfos) {
        if (!networkStoreService.networkExists(networkUuid)) {
            // The network is not loaded
            return CompletableFuture.completedFuture(Optional.empty());
        }

        if (!modificationGroupInfos.modifications().isEmpty()) {
            PreloadingStrategy preloadingStrategy = modificationGroupInfos.modifications().stream()
                .filter(m -> m.isActivatedOn(modificationGroupInfos.rootNetworkTag()))
                .map(ModificationInfos::getType)
                .map(ModificationTypeWithPreloadingStrategy::fromModificationType)
                .reduce(ModificationTypeWithPreloadingStrategy::maxStrategy)
                .map(ModificationTypeWithPreloadingStrategy::getStrategy)
                .orElse(PreloadingStrategy.NONE);
            NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, preloadingStrategy);

            // try to apply the duplicated modifications (incremental mode)
            if (networkInfos.isVariantPresent()) {
                return modificationApplicator.applyModifications(modificationGroupInfos, networkInfos).thenApply(Optional::of);
            }
        }
        return CompletableFuture.completedFuture(Optional.empty());
    }

    public CompletableFuture<NetworkModificationsResult> duplicateModifications(@NonNull UUID targetGroupUuid, UUID originGroupUuid, @NonNull List<UUID> modificationsUuids,
            @NonNull List<ModificationApplicationContext> applicationContexts) {
        if (originGroupUuid != null && !modificationsUuids.isEmpty()) { // Duplicate modifications from a group or from a list only
            throw new NetworkModificationServerException(MODIFICATION_DUPLICATION_ARGUMENT_ERROR, MODIFICATION_DUPLICATION_ARGUMENT_ERROR.messageTemplate());
        }
        List<ModificationInfos> duplicateModifications = networkModificationRepository.saveDuplicateModifications(targetGroupUuid, originGroupUuid,
            modificationsUuids, rootNetworkTagsOf(applicationContexts));
        List<UUID> ids = duplicateModifications.stream().map(ModificationInfos::getUuid).toList();
        return applyModifications(targetGroupUuid, duplicateModifications, applicationContexts).thenApply(result ->
            new NetworkModificationsResult(ids, result));
    }

    public void unshareModification(@NonNull UUID referenceModificationUuid, String userId) {
        // the group owning the reference, read before its deletion; null if the reference is inside a shared composite
        UUID groupUuid = modificationRepository.findGroupIdByModificationId(referenceModificationUuid);
        List<ModificationReferenceData> deletedReferences = networkModificationRepository.unshareModification(referenceModificationUuid);
        deleteSharingLinks(groupUuid, deletedReferences, userId);
    }

    /**
     * all their network modifications are extracted from the composite modifications and inserted into the group
     */
    public CompletableFuture<NetworkModificationsResult> splitCompositeModifications(
            @NonNull UUID targetGroupUuid,
            @NonNull Pair<List<CompositeInfos>, List<ModificationApplicationContext>> modificationContextInfos) {
        List<UUID> compositesUuids = modificationContextInfos.getFirst().stream().map(CompositeInfos::id).toList();
        List<ModificationInfos> modifications = networkModificationRepository.extractModificationsFromCompositesAndSave(targetGroupUuid, compositesUuids,
            rootNetworkTagsOf(modificationContextInfos.getSecond()));
        List<UUID> ids = modifications.stream().map(ModificationInfos::getUuid).toList();
        return applyModifications(targetGroupUuid, modifications, modificationContextInfos.getSecond()).thenApply(result ->
            new NetworkModificationsResult(ids, result));
    }

    public CompletableFuture<NetworkModificationsResult> insertCompositeModifications(
            @NonNull UUID targetGroupUuid,
            @NonNull Pair<List<CompositeInfos>, List<ModificationApplicationContext>> modificationContextInfos) {
        List<ModificationInfos> modifications = networkModificationRepository.insertCompositeModifications(targetGroupUuid,
            modificationContextInfos.getFirst(), rootNetworkTagsOf(modificationContextInfos.getSecond()));
        List<UUID> ids = modifications.stream().map(ModificationInfos::getUuid).toList();
        return applyModifications(targetGroupUuid, modifications, modificationContextInfos.getSecond()).thenApply(result ->
            new NetworkModificationsResult(ids, result));
    }

    @Transactional
    public UUID assembleNetworkModificationsIntoNewComposite(@NonNull List<UUID> assembledModificationsUuids, @NonNull String userId) {

        List<UUID> parentComposites = findModificationParentCompositesNonTransactional(assembledModificationsUuids).values().stream().toList();
        checkModificationsPermission(parentComposites, userId);
        CompositeModificationInfos newComposite =
                networkModificationRepository.assembleNetworkModificationsIntoNewComposite(assembledModificationsUuids).toModificationInfos();

        // the references among the assembled modifications are now located in the new composite
        UUID groupUuid = modificationRepository.findGroupIdByModificationId(newComposite.getUuid());
        notificationService.emitSharingLinksChanged(SharingLinkAction.UPDATE, groupUuid,
                getModificationsReferencesNonTransactional(assembledModificationsUuids, false), userId);

        // Collect referenced ancestor composites before assembling, since assembled modifications are moved out of their composite
        emitReferencedAncestorsUpdated(assembledModificationsUuids, userId);

        return newComposite.getUuid();
    }

    @Transactional
    public UUID createNetworkCompositeModification(@NonNull List<UUID> modificationUuids, @NonNull String name) {
        return networkModificationRepository.createNetworkCompositeModification(modificationUuids, name);
    }

    @Transactional
    public ModificationReferenceData extractCompositeModificationToShare(@NonNull UUID groupUuid, @NonNull UUID modificationUuid, String name, String description) {
        return networkModificationRepository.extractCompositeModificationToShare(groupUuid, modificationUuid, name, description);
    }

    public Map<UUID, UUID> duplicateCompositeModifications(List<UUID> sourceModificationUuids) {
        return networkModificationRepository.duplicateCompositeModifications(sourceModificationUuids);
    }

    public void updateModificationNameAndDescription(@NonNull UUID modificationUuid, ModificationMetadata modificationMetadata, String userId) {
        checkModificationsPermission(List.of(modificationUuid), userId);
        networkModificationRepository.updateModificationNameAndDescription(modificationUuid, modificationMetadata);
    }

    public void replaceCompositeModification(@NonNull UUID compositeUuid, String name, List<UUID> modificationUuids, @NonNull String userId) {
        checkModificationsPermission(List.of(compositeUuid), userId);
        // the deleted references were inside the composite, so in its group
        UUID groupUuid = modificationRepository.findGroupIdByModificationId(compositeUuid);
        List<ModificationReferenceData> deletedReferences = networkModificationRepository.replaceCompositeModification(compositeUuid, name, modificationUuids);
        deleteSharingLinks(groupUuid, deletedReferences, userId);

        // Notify directory-server if the replaced composite is itself referenced, then once per referenced ancestor composite (closest first)
        if (networkModificationRepository.isModificationReferenced(compositeUuid)) {
            notificationService.emitElementUpdated(compositeUuid, userId);
        }
    }

    @Transactional
    public void deleteStashedModificationFromGroups(List<UUID> groupUuids, @NonNull String userId) {
        // one deletion per group, so that the owner of each group is told about the references deleted from it
        groupUuids.forEach(groupUuid -> deleteSharingLinks(groupUuid,
                networkModificationRepository.deleteStashedModificationFromGroups(List.of(groupUuid)), userId));
    }

    public void deleteModificationReferences(@NonNull UUID groupUuid, @NonNull String userId) {
        List<ModificationReferenceData> deletedReferences = networkModificationRepository.deleteModificationReferences(groupUuid);
        deleteSharingLinks(groupUuid, deletedReferences, userId);
    }

    public List<ModificationMetadata> getModificationsMetadata(List<UUID> ids) {
        return networkModificationRepository.getModificationsMetadata(ids);
    }

    public static String escapeLucene(String s) {
        StringBuilder sb = new StringBuilder(s.length() + 16);

        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            if ("+\\-!()^[]\"{}~*?|&/ ".indexOf(c) != -1) {
                sb.append('\\');
            }
            sb.append(c);
        }
        return sb.toString();
    }

    public List<ModificationApplicationInfos> searchNetworkModificationInfos(@NonNull UUID networkUuid, @NonNull String userInput) {
        BoolQuery boolQueryBuilder = buildSearchModificationsQuery(userInput, networkUuid);

        NativeQuery nativeQuery = new NativeQueryBuilder()
                .withQuery(boolQueryBuilder._toQuery())
                .withPageable(PageRequest.of(0, PAGE_MAX_SIZE))
                .build();

        return elasticsearchOperations
                .search(nativeQuery, ModificationApplicationInfos.class)
                .stream()
                .map(SearchHit::getContent)
                .toList();
    }

    public Map<UUID, List<ModificationsSearchResult>> searchNetworkModifications(@NonNull UUID networkUuid, @NonNull String userInput) {
        List<ModificationApplicationInfos> rawSearchModificationInfos = searchNetworkModificationInfos(networkUuid, userInput);

        Map<UUID, ModificationEntity> modificationEntitiesById = getModificationEntitiesById(rawSearchModificationInfos);

        List<ModificationsSearchResult> filteredSearchModificationsResult = rawSearchModificationInfos.stream()
                .map(result -> findMatchingEquipmentResults(modificationEntitiesById.get(result.getModificationUuid()), result, userInput))
                .toList();

        return groupSearchResultsByGroupUuid(filteredSearchModificationsResult, rawSearchModificationInfos);
    }

    private Map<UUID, List<ModificationsSearchResult>> groupSearchResultsByGroupUuid(List<ModificationsSearchResult> modificationsSearchResults,
            List<ModificationApplicationInfos> modificationApplicationInfos) {
        Map<UUID, UUID> modificationToGroupMap = modificationApplicationInfos.stream()
                .collect(Collectors.toMap(
                        ModificationApplicationInfos::getModificationUuid,
                        ModificationApplicationInfos::getGroupUuid
                ));

        return modificationsSearchResults.stream()
                .collect(Collectors.groupingBy(
                        result -> modificationToGroupMap.get(result.getModificationUuid()),
                        Collectors.toList()
                ));
    }

    private Map<UUID, ModificationEntity> getModificationEntitiesById(List<ModificationApplicationInfos> modificationInfos) {
        return getModificationsByUuids(modificationInfos.stream()
                .map(ModificationApplicationInfos::getModificationUuid)
                .toList())
                .stream()
                .collect(Collectors.toMap(ModificationEntity::getId, Function.identity()));
    }

    private ModificationsSearchResult findMatchingEquipmentResults(
            ModificationEntity modificationEntity,
            ModificationApplicationInfos matchedModification,
            String userInput) {
        Pattern pattern = Pattern.compile(Pattern.quote(stripAccents(userInput)), Pattern.CASE_INSENSITIVE);

        List<String> impactedEquipmentIds = Stream.of(
                        matchedModification.getCreatedEquipmentIds(),
                        matchedModification.getModifiedEquipmentIds(),
                        matchedModification.getDeletedEquipmentIds()
                )
                .filter(Objects::nonNull)
                .flatMap(Collection::stream)
                .distinct()
                .filter(id -> pattern.matcher(stripAccents(id)).find())
                .toList();

        return ModificationsSearchResult.fromModificationEntity(modificationEntity)
                        .impactedEquipmentIds(impactedEquipmentIds)
                        .build();
    }

    private static String stripAccents(String input) {
        return StringUtils.stripAccents(input);
    }

    private BoolQuery buildSearchModificationsQuery(
            @NonNull String userInput,
            @NonNull UUID networkUuid) {

        String normalizedUserInput = "*" + escapeLucene(userInput) + "*";

        Query createdEquipmentQuery = Queries
                .wildcardQuery(CREATED_EQUIPMENT_IDS, normalizedUserInput)
                ._toQuery();

        Query modifiedEquipmentQuery = Queries
                .wildcardQuery(MODIFIED_EQUIPMENT_IDS, normalizedUserInput)
                ._toQuery();

        Query deletedEquipmentQuery = Queries
                .wildcardQuery(DELETED_EQUIPMENT_IDS, normalizedUserInput)
                ._toQuery();

        BoolQuery equipmentImpactedQuery = new BoolQuery.Builder()
                .should(List.of(createdEquipmentQuery, modifiedEquipmentQuery, deletedEquipmentQuery))
                .build();

        Query networkFilter = Queries
                .termQuery(NETWORK_UUID, networkUuid.toString())
                ._toQuery();

        BoolQuery.Builder boolQueryBuilder = new BoolQuery.Builder()
                .filter(List.of(equipmentImpactedQuery._toQuery(), networkFilter));

        return boolQueryBuilder.build();
    }

    public NetworkModificationsWithMissingInfo getNetworkModificationsFromCompositeWithMissingInfo(List<UUID> compositeModificationUuids) {
        Set<UUID> foundUuids = modificationRepository.findExistingCompositeModificationIds(compositeModificationUuids);
        List<ModificationInfos> networkModifications = networkModificationRepository.getCompositeModificationsInfos(compositeModificationUuids);
        modificationFilterNameService.addFilterNames(networkModifications);
        List<UUID> missingUuids = compositeModificationUuids.stream().filter(uuid -> !foundUuids.contains(uuid)).toList();
        return new NetworkModificationsWithMissingInfo(networkModifications, missingUuids);
    }

    public List<String> getBusBarSectionsForNewCoupler(@NonNull String voltageLevelId, @NonNull Integer busBarCount, @NonNull Integer sectionCount, List<SwitchKind> switchKindList) {
        List<String> bbsIds = new ArrayList<>();
        for (int i = 1; i < busBarCount + 1; i++) {
            for (int j = 1; j < sectionCount + 1; j++) {
                bbsIds.add(modificationApplicator.getNamingStrategy().getBusbarId(voltageLevelId, switchKindList, i, j));
            }
        }
        return bbsIds;
    }
}
