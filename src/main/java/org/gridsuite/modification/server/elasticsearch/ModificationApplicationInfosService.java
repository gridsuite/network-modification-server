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

import java.util.List;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
@Service
public class ModificationApplicationInfosService {
    private final ModificationApplicationInfosRepository basicModificationInfosRepository;
    private final ModificationApplicationRepository modificationApplicationRepository;

    public ModificationApplicationInfosService(ModificationApplicationInfosRepository basicModificationInfosRepository,
                                               ModificationApplicationRepository modificationApplicationRepository) {
        this.basicModificationInfosRepository = basicModificationInfosRepository;
        this.modificationApplicationRepository = modificationApplicationRepository;
    }

    public void addAll(List<ModificationApplicationInfos> basicModificationInfos) {
        modificationApplicationRepository.saveAll(basicModificationInfos.stream().map(modificationInfo -> {
            ModificationApplicationEntity newModificationApplicationEntity = ModificationApplicationEntity.builder()
                .networkUuid(modificationInfo.getNetworkUuid())
                .createdEquipmentIds(modificationInfo.getCreatedEquipmentIds())
                .modifiedEquipmentIds(modificationInfo.getModifiedEquipmentIds())
                .deletedEquipmentIds(modificationInfo.getDeletedEquipmentIds())
                .build();
            modificationInfo.getModification().addModificationApplication(newModificationApplicationEntity);
            return newModificationApplicationEntity;
        }).toList());
        basicModificationInfosRepository.saveAll(basicModificationInfos);
    }

    public void deleteAllByNetworkUuid(List<UUID> modificationUuids, UUID networkUuid) {
        modificationApplicationRepository.deleteAllByNetworkUuidAndModificationIdIn(networkUuid, modificationUuids);
        basicModificationInfosRepository.deleteAllByNetworkUuidAndModificationUuidIn(networkUuid, modificationUuids);
    }

    public void deleteAllByUuids(List<UUID> modificationUuids) {
        modificationApplicationRepository.deleteAllByModificationIdIn(modificationUuids);
        basicModificationInfosRepository.deleteAllByModificationUuidIn(modificationUuids);
    }
}
