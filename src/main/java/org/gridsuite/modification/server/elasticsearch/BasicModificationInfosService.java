package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.elasticsearch.BasicModificationInfos;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
public class BasicModificationInfosService {
    private final BasicModificationInfosRepository basicModificationInfosRepository;

    public BasicModificationInfosService(BasicModificationInfosRepository basicModificationInfosRepository) {
        this.basicModificationInfosRepository = basicModificationInfosRepository;
    }

    public void add(List<BasicModificationInfos> basicModificationInfos) {
        basicModificationInfosRepository.saveAll(basicModificationInfos);
    }

    public void deleteByNetworkUuid(List<UUID> modificationUuids, UUID networkUuid) {
        basicModificationInfosRepository.deleteAllByNetworkUuidAndModificationUuidIn(networkUuid, modificationUuids);
    }
}
