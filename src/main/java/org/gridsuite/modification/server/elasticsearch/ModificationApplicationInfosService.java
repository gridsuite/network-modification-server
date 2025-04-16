/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Service
public class ModificationApplicationInfosService {
    private final ModificationApplicationInfosRepository modificationApplicationInfosRepository;
    private final ModificationApplicationRepository modificationApplicationRepository;

    public ModificationApplicationInfosService(ModificationApplicationInfosRepository modificationApplicationInfosRepository,
                                               ModificationApplicationRepository modificationApplicationRepository) {
        this.modificationApplicationInfosRepository = modificationApplicationInfosRepository;
        this.modificationApplicationRepository = modificationApplicationRepository;
    }

    public void addAll(List<ModificationApplicationInfos> modificationApplicationInfos) {
        modificationApplicationRepository.saveAll(modificationApplicationInfos.stream().map(modificationInfo -> {
            ModificationApplicationEntity newModificationApplicationEntity = ModificationApplicationEntity.builder()
                .networkUuid(modificationInfo.getNetworkUuid())
                .createdEquipmentIds(modificationInfo.getCreatedEquipmentIds())
                .modifiedEquipmentIds(modificationInfo.getModifiedEquipmentIds())
                .deletedEquipmentIds(modificationInfo.getDeletedEquipmentIds())
                .build();
            newModificationApplicationEntity.setModification(modificationInfo.getModification());
            return newModificationApplicationEntity;
        }).toList());
        modificationApplicationInfosRepository.saveAll(modificationApplicationInfos);
    }

    public void deleteAllByGroupUuidsAndNetworkUuid(List<UUID> groupUuids, UUID networkUuid) {
        modificationApplicationRepository.deleteAllByNetworkUuidAndModificationGroupIdIn(networkUuid, groupUuids);
        modificationApplicationInfosRepository.deleteAllByNetworkUuidAndGroupUuidIn(networkUuid, groupUuids);
    }

    public void deleteAllByGroupUuids(List<UUID> groupUuids) {
        modificationApplicationRepository.deleteAllByModificationGroupIdIn(groupUuids);
        modificationApplicationInfosRepository.deleteAllByGroupUuidIn(groupUuids);
    }

    public void deleteAllByModificationIds(List<UUID> modificationIds) {
        modificationApplicationRepository.deleteAllByModificationIdIn(modificationIds);
        modificationApplicationInfosRepository.deleteAllByModificationUuidIn(modificationIds);
    }

    public void deleteAll() {
        modificationApplicationRepository.deleteAll();
        modificationApplicationInfosRepository.deleteAll();
    }

    @Transactional(readOnly = true)
    public long getIndexModificationsCount() {
        return modificationApplicationInfosRepository.count();
    }

    @Transactional(readOnly = true)
    public long getModificationsToReindexCount() {
        return modificationApplicationRepository.count();
    }

    @Transactional
    public void reindexAll() {
        modificationApplicationInfosRepository.deleteAll();
        modificationApplicationInfosRepository.saveAll(modificationApplicationRepository.findAll().stream().map(
            applicationInfo -> ModificationApplicationInfos.builder()
                .modificationUuid(applicationInfo.getModification().getId())
                .deletedEquipmentIds(applicationInfo.getDeletedEquipmentIds())
                .createdEquipmentIds(applicationInfo.getCreatedEquipmentIds())
                .modifiedEquipmentIds(applicationInfo.getModifiedEquipmentIds())
                .networkUuid(applicationInfo.getNetworkUuid())
                .groupUuid(applicationInfo.getModification().getGroup().getId())
                .build()
        ).toList());
    }
}
