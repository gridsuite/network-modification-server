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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;

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
    public List<ModificationInfos> getNetworkModifications(UUID groupUuid, boolean onlyMetadata, boolean errorOnGroupNotFound) {
        return networkModificationRepository.getModifications(groupUuid, onlyMetadata, errorOnGroupNotFound);
    }

    @Transactional(readOnly = true)
    public ModificationInfos getNetworkModification(UUID networkModificationUuid) {
        return networkModificationRepository.getModificationInfo(networkModificationUuid);
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

    // No transactional because we need to save modification in DB also in case of error
    // Transaction made in 'saveModifications' method
    // TODO Add transaction when errors will no longer be sent to the front
    public Optional<NetworkModificationResult> createNetworkModification(@NonNull UUID networkUuid, String variantId, @NonNull UUID groupUuid,
                                                                         @NonNull ReportInfos reportInfos,
                                                                         @NonNull ModificationInfos modificationInfos) {
        NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, modificationInfos.getType().getStrategy());

        networkModificationRepository.saveModifications(groupUuid, List.of(modificationInfos.toEntity()));

        return networkInfos.isVariantPresent() ?
            Optional.of(modificationApplicator.applyModifications(List.of(modificationInfos), networkInfos, reportInfos)) :
            Optional.empty();
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
        List<Pair<String, List<ModificationInfos>>> modificationInfos = new ArrayList<>();

        Streams.forEachPair(buildInfos.getModificationGroupUuids().stream(), buildInfos.getReporterIds().stream(),
            (groupUuid, reporterId) -> {
                List<ModificationInfos> modificationsByGroup = List.of();
                try {
                    modificationsByGroup = networkModificationRepository.getModificationsInfos(List.of(groupUuid));
                } catch (NetworkModificationException e) {
                    if (e.getType() != MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                        throw e;
                    }
                }
                modificationInfos.add(
                    Pair.of(reporterId,
                        modificationsByGroup.stream()
                            .filter(e -> !buildInfos.getModificationsToExclude().contains(e.getUuid()))
                            .collect(Collectors.toList()))
                );

            }
        );

        PreloadingStrategy preloadingStrategy = computePreloadingStrategy(modificationInfos.stream().map(Pair::getRight).flatMap(Collection::stream).collect(Collectors.toList()));
        Network network = cloneNetworkVariant(networkUuid, buildInfos.getOriginVariantId(), buildInfos.getDestinationVariantId(), preloadingStrategy);
        NetworkInfos networkInfos = new NetworkInfos(network, networkUuid, true);

        return modificationApplicator.applyModifications(modificationInfos, networkInfos, buildInfos.getReportUuid());
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

    private PreloadingStrategy computePreloadingStrategy(List<ModificationInfos> modificationInfos) {
        return modificationInfos.stream().map(m -> m.getType().getStrategy()).max(Comparator.comparing(Enum::ordinal)).orElse(PreloadingStrategy.NONE);
    }

    @Transactional
    public Optional<NetworkModificationResult> moveModifications(UUID groupUuid, UUID originGroupUuid,
                                                                 UUID before, UUID networkUuid, String variantId,
                                                                 ReportInfos reportInfos, List<UUID> modificationsToMove,
                                                                 boolean canBuildNode) {
        List<ModificationInfos> movedModifications = networkModificationRepository.moveModifications(groupUuid, originGroupUuid, modificationsToMove, before)
            .stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());

        PreloadingStrategy preloadingStrategy = computePreloadingStrategy(movedModifications);
        NetworkInfos networkInfos = getNetworkInfos(networkUuid, variantId, preloadingStrategy);

        if (canBuildNode && !movedModifications.isEmpty() && networkInfos.isVariantPresent()) { // TODO remove canBuildNode and return NetworkDamages() ?
            // try to apply the moved modifications (incremental mode)
            return Optional.of(modificationApplicator.applyModifications(
                    movedModifications,
                    networkInfos,
                    reportInfos));
        }
        return Optional.empty();
    }

    public void createModificationGroup(UUID sourceGroupUuid, UUID groupUuid) {
        try {
            networkModificationRepository.saveModifications(groupUuid, networkModificationRepository.copyModificationsEntities(sourceGroupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                return;
            }
            throw e;
        }
    }

    @Transactional
    public Optional<NetworkModificationResult> duplicateModifications(UUID targetGroupUuid,
                                                                      UUID networkUuid, String variantId,
                                                                      ReportInfos reportInfos, List<UUID> modificationsUuids) {
        List<ModificationEntity> modificationsEntities = networkModificationRepository.getModificationsEntities(modificationsUuids);
        List<ModificationEntity> duplicatedModificationsEntities = modificationsEntities.stream().map(ModificationEntity::copy).collect(Collectors.toList());
        if (!duplicatedModificationsEntities.isEmpty()) {
            networkModificationRepository.saveModifications(targetGroupUuid, duplicatedModificationsEntities);

            List<ModificationInfos> modificationInfos = duplicatedModificationsEntities.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList());
            PreloadingStrategy preloadingStrategy = computePreloadingStrategy(modificationInfos);
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
}
