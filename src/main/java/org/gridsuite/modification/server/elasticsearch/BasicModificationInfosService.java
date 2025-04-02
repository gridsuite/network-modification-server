package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.elasticsearch.BasicModificationInfos;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
public class BasicModificationInfosService {
    private final BasicModificationInfosRepository basicModificationInfosRepository;
    private final ModificationApplicationRepository modificationApplicationRepository;
    private final ModificationRepository modificationRepository;

    public BasicModificationInfosService(BasicModificationInfosRepository basicModificationInfosRepository,
                                         ModificationApplicationRepository modificationApplicationRepository,
                                         ModificationRepository modificationRepository) {
        this.basicModificationInfosRepository = basicModificationInfosRepository;
        this.modificationApplicationRepository = modificationApplicationRepository;
        this.modificationRepository = modificationRepository;
    }

    public void add(List<BasicModificationInfos> basicModificationInfos) {
        modificationApplicationRepository.saveAll(basicModificationInfos.stream().map(modificationInfo ->
            modificationRepository.findWithApplicationsById(modificationInfo.getModificationUuid()).map(modificationEntity -> {
                ModificationApplicationEntity newModificationApplicationEntity = ModificationApplicationEntity.builder()
                    .networkUuid(modificationInfo.getNetworkUuid())
                    .impactedEquipmentIds(modificationInfo.getImpactedEquipmentUuids())
                    .build();
                modificationEntity.addModificationApplication(newModificationApplicationEntity);
                return newModificationApplicationEntity;
            })
        ).filter(Optional::isPresent).map(Optional::get).toList());

        basicModificationInfosRepository.saveAll(basicModificationInfos);
    }

    public void deleteByNetworkUuid(List<UUID> modificationUuids, UUID networkUuid) {
        modificationApplicationRepository.deleteAllByNetworkUuidAndModificationIdIn(networkUuid, modificationUuids);
        basicModificationInfosRepository.deleteAllByNetworkUuidAndModificationUuidIn(networkUuid, modificationUuids);
    }

    public void deleteByUuids(List<UUID> modificationUuids) {
        modificationApplicationRepository.deleteAllByModificationIdIn(modificationUuids);
        basicModificationInfosRepository.deleteAllByModificationUuidIn(modificationUuids);
    }
}
