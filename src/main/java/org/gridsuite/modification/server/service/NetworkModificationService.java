/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Streams;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.server.NetworkModificationServerException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.NetworkModificationServerException.Type.DUPLICATION_ARGUMENT_INVALID;

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

    public NetworkModificationService(NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      EquipmentInfosService equipmentInfosService, NotificationService notificationService,
                                      NetworkModificationApplicator applicationService, ObjectMapper objectMapper) {
        this.networkStoreService = networkStoreService;
        this.networkModificationRepository = networkModificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.notificationService = notificationService;
        this.modificationApplicator = applicationService;
        this.objectMapper = objectMapper;
    }

    public List<UUID> getModificationGroups() {
        return networkModificationRepository.getModificationGroupsUuids();
    }

    @Transactional(readOnly = true)
    // Need a transaction for collections lazy loading
    public List<ModificationInfos> getNetworkModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound, boolean stashedModifications) {
        return networkModificationRepository.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound, stashedModifications);
    }

    @Transactional(readOnly = true)
    // Need a transaction for collections lazy loading
    public List<ModificationInfos> getNetworkModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return getNetworkModifications(groupUuid, onlyMetadata, errorOnGroupNotFound, false);
    }

    @Transactional(readOnly = true)
    public List<ModificationInfos> getNetworkModificationsFromComposite(UUID compositeModificationUuid, boolean onlyMetadata) {
        if (onlyMetadata) {
            return networkModificationRepository.getBasicNetworkModificationsFromComposite(compositeModificationUuid);
        } else {
            return networkModificationRepository.getCompositeModificationsInfos(List.of(compositeModificationUuid));
        }
    }

    @Transactional(readOnly = true)
    public ModificationInfos getNetworkModification(UUID networkModificationUuid) {
        return networkModificationRepository.getModificationInfo(networkModificationUuid);
    }

    public Integer getNetworkModificationsCount(UUID groupUuid, boolean stashed) {
        return networkModificationRepository.getModificationsCount(groupUuid, stashed);
    }

    public void deleteModificationGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        networkModificationRepository.deleteModificationGroup(groupUuid, errorOnGroupNotFound);
    }

    public NetworkInfos getNetworkInfos(UUID networkUuid, String variantId, PreloadingStrategy preloadingStrategy) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid, preloadingStrategy);
        } catch (PowsyblException e) {
            throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
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

    @Transactional
    public void updateNetworkModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        networkModificationRepository.updateModification(modificationUuid, modificationInfos);
    }

    @Transactional
    public void updateNetworkModificationActivation(@NonNull List<UUID> modificationUuids, boolean activated) {
        networkModificationRepository.updateNetworkModificationsActivation(modificationUuids, activated);
    }

    @Transactional
    public void stashNetworkModifications(UUID groupUuid, @NonNull List<UUID> modificationUuids) {
        networkModificationRepository.stashNetworkModifications(modificationUuids, networkModificationRepository.getModificationsCount(groupUuid, true));
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

    // No transactional because we need to save modification in DB also in case of error
    // Transaction made in 'saveModifications' method
    // TODO Add transaction when errors will no longer be sent to the front
    public List<Optional<NetworkModificationResult>> createNetworkModification(@NonNull UUID groupUuid, @NonNull ModificationInfos modificationInfo, @NonNull List<ModificationApplicationContext> applicationContexts) {
        networkModificationRepository.saveModificationInfos(groupUuid, List.of(modificationInfo)).stream().map(ModificationEntity::getId).findFirst();

        return applyModifications(List.of(modificationInfo), applicationContexts);
    }

    /**
     * TODO : Remove this method after the final integration of root networks
     * Need to use tne new method with ModificationApplicationContext DTO (see above)
     */
    public Optional<NetworkModificationResult> createNetworkModification(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid,
                                                                         @NonNull ReportInfos reportInfos,
                                                                         @NonNull ModificationInfos modificationInfos) {
        NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, modificationInfos.getType().getStrategy());

        networkModificationRepository.saveModificationInfos(groupUuid, List.of(modificationInfos));

        return networkInfos.isVariantPresent() ?
            Optional.of(modificationApplicator.applyModifications(List.of(modificationInfos), networkInfos, reportInfos)) :
            Optional.empty();
    }

    /**
     * Apply modifications on several networks
     */
    private List<Optional<NetworkModificationResult>> applyModifications(List<ModificationInfos> modifications, List<ModificationApplicationContext> applicationContexts) {
        return applicationContexts.stream().map(modificationApplicationContext ->
            applyModifications(modificationApplicationContext.networkUuid(),
                modificationApplicationContext.variantId(),
                new ReportInfos(modificationApplicationContext.reportUuid(), modificationApplicationContext.reporterId()),
                modifications)
        ).toList();
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
            throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
        }
        String startingVariant = StringUtils.isBlank(originVariantId) ? VariantManagerConstants.INITIAL_VARIANT_ID : originVariantId;
        try {
            network.getVariantManager().cloneVariant(startingVariant, destinationVariantId, true);  // cloning variant
            network.getVariantManager().setWorkingVariant(destinationVariantId);  // set current variant to destination variant
        } catch (PowsyblException e) {
            throw new NetworkModificationException(VARIANT_NOT_FOUND, startingVariant);
        }
        return network;
    }

    @Transactional(readOnly = true)
    public NetworkModificationResult buildVariant(@NonNull UUID networkUuid, @NonNull BuildInfos buildInfos) {
        // Apply all modifications belonging to the modification groups uuids in buildInfos
        List<Pair<ReportInfos, List<ModificationInfos>>> modificationInfos = new ArrayList<>();

        Streams.forEachPair(buildInfos.getModificationGroupUuids().stream(), buildInfos.getReportsInfos().stream(),
            (groupUuid, reporterId) -> {
                List<ModificationInfos> modificationsByGroup = List.of();
                try {
                    modificationsByGroup = networkModificationRepository.getModificationsInfos(List.of(groupUuid), false)
                        .stream()
                        .filter(m -> !m.getStashed())
                        .collect(Collectors.toList());
                } catch (NetworkModificationException e) {
                    if (e.getType() != MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                        throw e;
                    }
                }
                modificationInfos.add(
                    Pair.of(reporterId,
                        modificationsByGroup)
                );

            }
        );

        PreloadingStrategy preloadingStrategy = modificationInfos.stream().map(Pair::getRight)
                .flatMap(Collection::stream)
                .map(ModificationInfos::getType)
                .reduce(ModificationType::maxStrategy).map(ModificationType::getStrategy).orElse(PreloadingStrategy.NONE);

        Network network = cloneNetworkVariant(networkUuid, buildInfos.getOriginVariantId(), buildInfos.getDestinationVariantId(), preloadingStrategy);
        NetworkInfos networkInfos = new NetworkInfos(network, networkUuid, true);

        return modificationApplicator.applyModifications(modificationInfos, networkInfos);
    }

    public void buildVariantRequest(UUID networkUuid, BuildInfos buildInfos, String receiver) {
        notificationService.emitBuildMessage(new BuildExecContext(networkUuid, buildInfos, receiver).toMessage(objectMapper));
    }

    public void stopBuildRequest(String receiver) {
        notificationService.emitCancelBuildMessage(receiver);
    }

    public void deleteNetworkModifications(UUID groupUuid, List<UUID> modificationsUuids) {
        if (networkModificationRepository.deleteModifications(groupUuid, modificationsUuids) == 0) {
            throw new NetworkModificationException(MODIFICATION_NOT_FOUND);
        }
    }

    @Transactional
    public List<Optional<NetworkModificationResult>> moveModifications(@NonNull UUID destinationGroupUuid, @NonNull UUID originGroupUuid, UUID beforeModificationUuid,
                                                                       @NonNull List<UUID> modificationsToMoveUuids, @NonNull List<ModificationApplicationContext> applicationContexts,
                                                                       boolean canBuildNode) {
        // update origin/destinations groups to cut and paste all modificationsToMove
        List<ModificationInfos> modificationInfos = networkModificationRepository.moveModifications(destinationGroupUuid, originGroupUuid, modificationsToMoveUuids, beforeModificationUuid).stream()
            .map(networkModificationRepository::getModificationInfos)
            .toList();

        return canBuildNode && !modificationInfos.isEmpty() ? applyModifications(modificationInfos, applicationContexts) : List.of();
    }

    /**
     * TODO : Remove this method after the final integration of root networks
     * Need to use the new method with ModificationApplicationContext DTO (see above)
     */
    @Transactional
    public Optional<NetworkModificationResult> moveModifications(UUID destinationGroupUuid, UUID originGroupUuid,
                                                                 UUID beforeModificationUuid, UUID networkUuid, String variantId,
                                                                 ReportInfos reportInfos, List<UUID> modificationsToMove, boolean canBuildNode) {
        // update origin/destinations groups to cut and paste all modificationsToMove
        List<ModificationEntity> movedEntities = networkModificationRepository.moveModifications(destinationGroupUuid, originGroupUuid, modificationsToMove, beforeModificationUuid);

        if (canBuildNode && !movedEntities.isEmpty()) { // TODO remove canBuildNode ?
            // try to apply the moved modifications only (incremental mode)
            PreloadingStrategy preloadingStrategy = movedEntities.stream()
                .map(e -> ModificationType.valueOf(e.getType()))
                .reduce(ModificationType::maxStrategy).map(ModificationType::getStrategy).orElse(PreloadingStrategy.NONE);
            NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, preloadingStrategy);
            if (networkInfos.isVariantPresent()) {
                List<ModificationInfos> movedModifications = movedEntities.stream()
                    .map(networkModificationRepository::getModificationInfos).toList();
                return Optional.of(modificationApplicator.applyModifications(
                    movedModifications,
                    networkInfos,
                    reportInfos));
            }
        }
        return Optional.empty();
    }

    public void duplicateGroup(UUID sourceGroupUuid, UUID groupUuid) {
        try {
            networkModificationRepository.saveModificationInfos(groupUuid, networkModificationRepository.getActiveModificationsInfos(sourceGroupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                return;
            }
            throw e;
        }
    }

    private Optional<NetworkModificationResult> applyModifications(UUID networkUuid, String variantId,
                                                                   ReportInfos reportInfos, List<ModificationInfos> modificationInfos) {
        if (!modificationInfos.isEmpty()) {
            PreloadingStrategy preloadingStrategy = modificationInfos.stream()
                .map(ModificationInfos::getType)
                .reduce(ModificationType::maxStrategy).map(ModificationType::getStrategy).orElse(PreloadingStrategy.NONE);
            NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, preloadingStrategy);

            // try to apply the duplicated modifications (incremental mode)
            if (networkInfos.isVariantPresent()) {
                return Optional.of(modificationApplicator.applyModifications(
                    modificationInfos,
                    networkInfos,
                    reportInfos
                ));
            }
        }
        return Optional.empty();
    }

    @Transactional
    public List<Optional<NetworkModificationResult>> duplicateModifications(@NonNull UUID targetGroupUuid, UUID originGroupUuid, @NonNull List<UUID> modificationsUuids, @NonNull List<ModificationApplicationContext> applicationContexts) {
        if (originGroupUuid != null && !modificationsUuids.isEmpty()) { // Duplicate modifications from a group or from a list only
            throw new NetworkModificationServerException(DUPLICATION_ARGUMENT_INVALID);
        }
        List<ModificationInfos> modificationInfos = originGroupUuid != null ? networkModificationRepository.getActiveModificationsInfos(originGroupUuid) : networkModificationRepository.getModificationsInfos(modificationsUuids);
        networkModificationRepository.saveModificationInfos(targetGroupUuid, modificationInfos);
        return applyModifications(modificationInfos, applicationContexts);
    }

    /**
     * TODO : Remove this method after the final integration of root networks
     * Need to use the new method with ModificationApplicationContext DTO (see above)
     */
    @Transactional
    public Optional<NetworkModificationResult> duplicateModifications(UUID targetGroupUuid,
                                                                      UUID networkUuid, String variantId,
                                                                      ReportInfos reportInfos, List<UUID> modificationsUuids) {
        List<ModificationInfos> modificationInfos = networkModificationRepository.getModificationsInfos(modificationsUuids);
        networkModificationRepository.saveModificationInfos(targetGroupUuid, modificationInfos);
        return applyModifications(networkUuid, variantId, reportInfos, modificationInfos);
    }

    @Transactional
    public List<Optional<NetworkModificationResult>> insertCompositeModifications(@NonNull UUID targetGroupUuid, @NonNull List<UUID> modificationsUuids, @NonNull List<ModificationApplicationContext> applicationContexts) {
        List<ModificationInfos> modificationInfos = networkModificationRepository.getCompositeModificationsInfos(modificationsUuids);
        networkModificationRepository.saveModificationInfos(targetGroupUuid, modificationInfos);
        return applyModifications(modificationInfos, applicationContexts);
    }

    /**
     * TODO : Remove this method after the final integration of root networks (used only for move)
     * Need to use the new method with ModificationApplicationContext DTO (see above)
     */
    @Transactional
    public Optional<NetworkModificationResult> insertCompositeModifications(UUID targetGroupUuid,
                                                                            UUID networkUuid, String variantId,
                                                                            ReportInfos reportInfos, List<UUID> modificationsUuids) {
        List<ModificationInfos> modificationInfos = networkModificationRepository.getCompositeModificationsInfos(modificationsUuids);
        networkModificationRepository.saveModificationInfos(targetGroupUuid, modificationInfos);
        return applyModifications(networkUuid, variantId, reportInfos, modificationInfos);
    }

    @Transactional
    public UUID createNetworkCompositeModification(@NonNull List<UUID> modificationUuids) {
        return networkModificationRepository.createNetworkCompositeModification(modificationUuids);
    }

    public Map<UUID, UUID> duplicateModifications(List<UUID> sourceModificationUuids) {
        return networkModificationRepository.duplicateModifications(sourceModificationUuids);
    }

    public void deleteStashedModificationInGroup(UUID groupUuid, boolean errorOnGroupNotFound) {
        networkModificationRepository.deleteStashedModificationInGroup(groupUuid, errorOnGroupNotFound);
    }

    public List<ModificationMetadata> getModificationsMetadata(List<UUID> ids) {
        return networkModificationRepository.getModificationsMetadata(ids);
    }

    @Transactional
    public Optional<NetworkModificationResult> applyModificationsFromUuids(UUID networkUuid,
                                                                           String variantId,
                                                                           ReportInfos reportInfos,
                                                                           List<UUID> modificationsUuids) {
        List<ModificationInfos> modificationInfos = networkModificationRepository.getCompositeModificationsInfos(modificationsUuids);
        return applyModifications(networkUuid, variantId, reportInfos, modificationInfos);
    }
}
