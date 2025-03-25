package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.elasticsearch.BasicModificationInfos;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

import java.util.List;
import java.util.UUID;

public interface BasicModificationInfosRepository extends ElasticsearchRepository<BasicModificationInfos, String> {
    void deleteAllByNetworkUuidAndModificationUuidIn(UUID networkUuid, List<UUID> modificationUuid);

    void deleteAllByModificationUuidIn(List<UUID> modificationUuid);
}
