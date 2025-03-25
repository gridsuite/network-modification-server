package org.gridsuite.modification.server.elasticsearch;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.gridsuite.modification.server.dto.elasticsearch.BasicModificationInfos;
import org.gridsuite.modification.server.entities.ModificationBackupEntity;
import org.gridsuite.modification.server.repositories.ModificationBackupRepository;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

@Service
public class BasicModificationInfosService {
    private final BasicModificationInfosRepository basicModificationInfosRepository;
    private final ModificationBackupRepository modificationBackupRepository;
    private final ModificationRepository modificationRepository;
    private final ObjectWriter objectWriter = new ObjectMapper().writer().withDefaultPrettyPrinter();

    public BasicModificationInfosService(BasicModificationInfosRepository basicModificationInfosRepository,
                                         ModificationBackupRepository modificationBackupRepository,
                                         ModificationRepository modificationRepository) {
        this.basicModificationInfosRepository = basicModificationInfosRepository;
        this.modificationBackupRepository = modificationBackupRepository;
        this.modificationRepository = modificationRepository;
    }

    public void add(List<BasicModificationInfos> basicModificationInfos) {
        modificationBackupRepository.saveAll(basicModificationInfos.stream().map(modificationInfo ->
            modificationRepository.findById(modificationInfo.getModificationUuid()).map(modificationEntity -> {
                    try {
                        return ModificationBackupEntity.builder()
                            .networkUuid(modificationInfo.getNetworkUuid())
                            .modification(modificationEntity)
                            .indexInfos(objectWriter.writeValueAsString(modificationInfo))
                            .build();
                    } catch (JsonProcessingException e) {
                        //TODO: what to do ?
                        return null;
                    }
                }
            )
        ).filter(Optional::isPresent).map(Optional::get).toList());

        basicModificationInfosRepository.saveAll(basicModificationInfos);
    }

    public void deleteByNetworkUuid(List<UUID> modificationUuids, UUID networkUuid) {
        modificationBackupRepository.deleteAllByNetworkUuidAndModificationIdIn(networkUuid, modificationUuids);
        basicModificationInfosRepository.deleteAllByNetworkUuidAndModificationUuidIn(networkUuid, modificationUuids);
    }

    public void deleteByUuids(List<UUID> modificationUuids) {
        modificationBackupRepository.deleteAllByModificationIdIn(modificationUuids);
        basicModificationInfosRepository.deleteAllByModificationUuidIn(modificationUuids);
    }
}
