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
import com.powsybl.commons.reporter.ReporterModel;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.VariantManagerConstants;
import com.powsybl.network.store.client.NetworkStoreService;
import lombok.NonNull;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
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

    // TO DO : transfer the use of repositories in NetworkModificationRepository
    private final ModificationRepository modificationRepository;

    private final EquipmentInfosService equipmentInfosService;

    private final NotificationService notificationService;

    private final ReportService reportService;

    private final ObjectMapper objectMapper;

    public NetworkModificationService(NetworkStoreService networkStoreService, NetworkModificationRepository networkModificationRepository,
                                      EquipmentInfosService equipmentInfosService,
                                      ModificationRepository modificationRepository, NotificationService notificationService, ReportService reportService,
                                      NetworkModificationApplicator applicationService, ObjectMapper objectMapper) {
        this.networkStoreService = networkStoreService;
        this.networkModificationRepository = networkModificationRepository;
        this.equipmentInfosService = equipmentInfosService;
        this.modificationRepository = modificationRepository;
        this.notificationService = notificationService;
        this.modificationApplicator = applicationService;
        this.reportService = reportService;
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

    public NetworkInfos getNetworkInfos(UUID networkUuid, String variantId) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid);
        } catch (PowsyblException e) {
            throw new NetworkModificationException(NETWORK_NOT_FOUND, networkUuid.toString());
        }
        boolean isVariantExist = true;
        if (variantId != null) {
            if (network.getVariantManager().getVariantIds().stream().anyMatch(id -> id.equals(variantId))) {
                network.getVariantManager().setWorkingVariant(variantId);
            } else {
                isVariantExist = false;
            }
        }
        return new NetworkInfos(network, networkUuid, isVariantExist);
    }

    @Transactional
    public void updateNetworkModification(@NonNull UUID modificationUuid, @NonNull ModificationInfos modificationInfos) {
        ModificationEntity modificationEntity = this.modificationRepository.findById(modificationUuid)
            .orElseThrow(() -> new NetworkModificationException(MODIFICATION_NOT_FOUND, String.format("Modification (%s) not found", modificationUuid)));

        modificationEntity.update(modificationInfos);
    }

    // No transactional because we need to save modification in DB also in case of error
    // Transaction made in 'saveModifications' method
    public List<ModificationInfos> createNetworkModification(@NonNull NetworkInfos networkInfos, @NonNull UUID groupUuid,
                                                             @NonNull ReportInfos reportInfos,
                                                             @NonNull ModificationInfos modificationInfos) {
        networkModificationRepository.saveModifications(groupUuid, List.of(modificationInfos.toEntity()));

        return networkInfos.isVariantExist() ? modificationApplicator.applyModification(modificationInfos, networkInfos, reportInfos) : List.of();
    }

    public Network cloneNetworkVariant(UUID networkUuid, String originVariantId, String destinationVariantId) {
        Network network;
        try {
            network = networkStoreService.getNetwork(networkUuid);
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
    public List<ModificationInfos> buildVariant(@NonNull NetworkInfos networkInfos, @NonNull BuildInfos buildInfos) {
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

                if (modificationsByGroup.isEmpty()) {
                    reportService.sendReport(buildInfos.getReportUuid(), new ReporterModel(reporterId, reporterId));
                } else {
                    modificationInfos.add(
                        Pair.of(reporterId,
                            modificationsByGroup.stream()
                                .filter(e -> !buildInfos.getModificationsToExclude().contains(e.getUuid()))
                                .collect(Collectors.toList()))
                    );
                }
            }
        );

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

    public void moveModifications(UUID groupUuid, UUID originGroupUuid, UUID before, NetworkInfos networkInfos, ReportInfos reportInfos, List<UUID> modificationsToMove, boolean canBuildNode) {
        List<ModificationInfos> movedModifications = networkModificationRepository.moveModifications(groupUuid, originGroupUuid, modificationsToMove, before)
            .stream()
            .map(ModificationEntity::toModificationInfos)
            .collect(Collectors.toList());
        if (canBuildNode && !movedModifications.isEmpty() && networkInfos.isVariantExist()) { // TODO remove canBuildNode and return NetworkDamages() ?
            // try to apply the moved modifications (incremental mode)
            modificationApplicator.applyModifications(movedModifications, networkInfos, reportInfos);
        }
    }

    public void createModificationGroup(UUID sourceGroupUuid, UUID groupUuid) {
        try {
            networkModificationRepository.saveModifications(groupUuid, networkModificationRepository.cloneModificationsEntities(sourceGroupUuid));
        } catch (NetworkModificationException e) {
            if (e.getType() == MODIFICATION_GROUP_NOT_FOUND) { // May not exist
                return;
            }
            throw e;
        }
    }

    // This function cannot be @Transactional because we clone all modifications resetting their id to null,
    // which is not allowed by JPA if we still stay in the same Tx.
    public List<UUID> duplicateModifications(UUID targetGroupUuid, NetworkInfos networkInfos, ReportInfos reportInfos, List<UUID> modificationsUuidList) {
        List<ModificationEntity> duplicatedModificationEntityList = new ArrayList<>();
        List<UUID> missingModificationList = new ArrayList<>();
        for (UUID modifyId : modificationsUuidList) {
            networkModificationRepository.cloneModificationEntity(modifyId).ifPresentOrElse(
                duplicatedModificationEntityList::add,
                () -> missingModificationList.add(modifyId)  // data no more available
            );
        }
        if (!duplicatedModificationEntityList.isEmpty()) {
            networkModificationRepository.saveModifications(targetGroupUuid, duplicatedModificationEntityList);
            // try to apply the duplicated modifications (incremental mode)
            if (networkInfos.isVariantExist()) { // TODO return NetworkDamages() ?
                modificationApplicator.applyModifications(
                    duplicatedModificationEntityList.stream().map(ModificationEntity::toModificationInfos).collect(Collectors.toList()),
                    networkInfos, reportInfos
                );
            }
        }
        return missingModificationList;
    }
}
