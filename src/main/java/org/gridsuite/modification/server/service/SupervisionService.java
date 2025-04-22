package org.gridsuite.modification.server.service;

import lombok.Getter;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.ESConfig;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.IndexOperations;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;

@Service
public class SupervisionService {
    private final ModificationApplicationInfosRepository modificationApplicationInfosRepository;
    private final ModificationApplicationRepository modificationApplicationRepository;
    private final ElasticsearchOperations elasticsearchOperations;

    @Value(ESConfig.MODIFICATIONS_INDEX_NAME)
    @Getter
    private String modificationIndexName;

    public SupervisionService(ModificationApplicationInfosRepository modificationApplicationInfosRepository,
                              ModificationApplicationRepository modificationApplicationRepository,
                              ElasticsearchOperations elasticsearchOperations) {
        this.modificationApplicationInfosRepository = modificationApplicationInfosRepository;
        this.modificationApplicationRepository = modificationApplicationRepository;
        this.elasticsearchOperations = elasticsearchOperations;
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
        modificationApplicationInfosRepository.saveAll(modificationApplicationRepository.findAllWithModificationAndGroup().stream().map(
            ModificationApplicationEntity::toModificationApplicationInfos
        ).toList());
    }

    public void recreateIndex() {
        IndexOperations indexOperations = elasticsearchOperations.indexOps(ModificationApplicationInfos.class);
        boolean isDeleted = indexOperations.delete();
        if (!isDeleted) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR,
                "Failed to delete modifications ElasticSearch index");
        }

        boolean isCreated = indexOperations.createWithMapping();
        if (!isCreated) {
            throw new ResponseStatusException(HttpStatus.INTERNAL_SERVER_ERROR,
                "Failed to create modifications ElasticSearch index");
        }
    }
}
