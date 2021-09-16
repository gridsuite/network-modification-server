/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.elasticsearch.index.query.QueryBuilders;
import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * A class to implement elasticsearch indexing
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public class EquipmentInfosServiceImpl implements EquipmentInfosService {

    private final EquipmentInfosRepository equipmentInfosRepository;

    private final ElasticsearchOperations elasticsearchOperations;

    public EquipmentInfosServiceImpl(EquipmentInfosRepository equipmentInfosRepository, ElasticsearchOperations elasticsearchOperations) {
        this.equipmentInfosRepository = equipmentInfosRepository;
        this.elasticsearchOperations = elasticsearchOperations;
    }

    @Override
    public List<EquipmentInfos> search(@NonNull final String query) {
        SearchHits<EquipmentInfos> searchHits = elasticsearchOperations.search(new NativeSearchQuery(QueryBuilders.queryStringQuery(query)), EquipmentInfos.class);
        return searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
    }

    @Override
    public void deleteAll(@NonNull UUID networkUuid) {
        equipmentInfosRepository.deleteAllByNetworkUuid(networkUuid);
    }
}
