/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import com.google.common.collect.Lists;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.DatabaseConstants.SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE;
import static org.gridsuite.modification.server.utils.DatabaseConstants.SQL_SUB_MODIFICATION_SAVE_BATCH_SIZE;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Service
public class ModificationApplicationInfosService {
    private final ModificationApplicationInfosRepository modificationApplicationInfosRepository;
    private final ModificationApplicationRepository modificationApplicationRepository;
    @Value("${spring.data.elasticsearch.partition-size:10000}")
    private int partitionSize;
    @Value("${spring.data.elasticsearch.partition-size-for-deletion:2048}")
    public int partitionSizeForDeletion;

    public ModificationApplicationInfosService(ModificationApplicationInfosRepository modificationApplicationInfosRepository,
                                               ModificationApplicationRepository modificationApplicationRepository) {
        this.modificationApplicationInfosRepository = modificationApplicationInfosRepository;
        this.modificationApplicationRepository = modificationApplicationRepository;
    }

    public void addAll(List<ModificationApplicationInfos> modificationApplicationInfos) {
        Lists.partition(modificationApplicationInfos, SQL_SUB_MODIFICATION_SAVE_BATCH_SIZE)
            .parallelStream()
            .forEach(modificationApplicationInfosBatch ->
                modificationApplicationRepository.saveAll(modificationApplicationInfosBatch.stream()
                    .map(modificationInfo -> {
                        ModificationApplicationEntity newModificationApplicationEntity = ModificationApplicationEntity
                            .builder()
                            .networkUuid(modificationInfo.getNetworkUuid())
                            .createdEquipmentIds(modificationInfo.getCreatedEquipmentIds())
                            .modifiedEquipmentIds(modificationInfo.getModifiedEquipmentIds())
                            .deletedEquipmentIds(modificationInfo.getDeletedEquipmentIds())
                            .build();
                        newModificationApplicationEntity.setModification(modificationInfo.getModification());
                        return newModificationApplicationEntity;
                    }).toList()));
        Lists.partition(modificationApplicationInfos, partitionSize)
            .parallelStream()
            .forEach(modificationApplicationInfosRepository::saveAll);
    }

    public void deleteAllByGroupUuidsAndNetworkUuid(List<UUID> groupUuids, UUID networkUuid) {
        Lists.partition(groupUuids, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).parallelStream().forEach(ids ->
            modificationApplicationRepository.deleteAllByNetworkUuidAndModificationGroupIdIn(networkUuid, ids));
        Lists.partition(groupUuids, partitionSizeForDeletion).parallelStream().forEach(ids ->
            modificationApplicationInfosRepository.deleteAllByNetworkUuidAndGroupUuidIn(networkUuid, ids));
    }

    public void deleteAllByGroupUuids(List<UUID> groupUuids) {
        Lists.partition(groupUuids, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).parallelStream()
            .forEach(modificationApplicationRepository::deleteAllByModificationGroupIdIn);
        Lists.partition(groupUuids, partitionSizeForDeletion).parallelStream()
            .forEach(modificationApplicationInfosRepository::deleteAllByGroupUuidIn);
    }

    public void deleteAllByModificationIds(List<UUID> modificationIds) {
        Lists.partition(modificationIds, SQL_SUB_MODIFICATION_DELETION_BATCH_SIZE).parallelStream()
            .forEach(modificationApplicationRepository::deleteAllByModificationIdIn);
        Lists.partition(modificationIds, partitionSizeForDeletion).parallelStream()
            .forEach(modificationApplicationInfosRepository::deleteAllByModificationUuidIn);
    }

    public void deleteAll() {
        modificationApplicationRepository.deleteAll();
        modificationApplicationInfosRepository.deleteAll();
    }
}
