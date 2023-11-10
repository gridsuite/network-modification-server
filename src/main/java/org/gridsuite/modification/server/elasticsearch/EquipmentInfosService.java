/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import co.elastic.clients.elasticsearch._types.query_dsl.*;
import com.google.common.collect.Lists;
import com.powsybl.iidm.network.Identifiable;
import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.SubstationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.TombstonedEquipmentInfos;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * A class to implement elasticsearch indexing
 *
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Nicolas Noir <nicolas.noir at rte-france.com>
 */

@Service
public class EquipmentInfosService {
    private final EquipmentInfosRepository equipmentInfosRepository;

    private final TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository;

    private final ElasticsearchOperations elasticsearchOperations;

    private static final int PAGE_MAX_SIZE = 400;

    @Value("${spring.data.elasticsearch.partition-size:10000}")
    private int partitionSize;

    public EquipmentInfosService(EquipmentInfosRepository equipmentInfosRepository, TombstonedEquipmentInfosRepository tombstonedEquipmentInfosRepository, ElasticsearchOperations elasticsearchOperations) {
        this.equipmentInfosRepository = equipmentInfosRepository;
        this.tombstonedEquipmentInfosRepository = tombstonedEquipmentInfosRepository;
        this.elasticsearchOperations = elasticsearchOperations;
    }

    public void addAllEquipmentInfos(@NonNull final List<EquipmentInfos> equipmentsInfos) {
        Lists.partition(equipmentsInfos, partitionSize)
                .parallelStream()
                .forEach(equipmentInfosRepository::saveAll);
    }

    public void addAllTombstonedEquipmentInfos(@NonNull final List<TombstonedEquipmentInfos> tombstonedEquipmentInfos) {
        Lists.partition(tombstonedEquipmentInfos, partitionSize)
                .parallelStream()
                .forEach(tombstonedEquipmentInfosRepository::saveAll);
    }

    public void deleteEquipmentInfosList(@NonNull List<String> equipmentIds, @NonNull UUID networkUuid, @NonNull String variantId) {
        equipmentInfosRepository.deleteByIdInAndNetworkUuidAndVariantId(equipmentIds, networkUuid, variantId);
    }

    public void deleteVariants(@NonNull UUID networkUuid, List<String> variantIds) {
        variantIds.forEach(variantId -> {
            equipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
            tombstonedEquipmentInfosRepository.deleteAllByNetworkUuidAndVariantId(networkUuid, variantId);
        });
    }

    public void cloneVariantModifications(@NonNull UUID networkUuid, @NonNull String variantToCloneId, @NonNull String variantId) {
        addAllEquipmentInfos(
                equipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).stream()
                        .map(equipmentInfos -> {
                            equipmentInfos.setUniqueId(null);
                            equipmentInfos.setVariantId(variantId);
                            return equipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
        addAllTombstonedEquipmentInfos(
                tombstonedEquipmentInfosRepository.findAllByNetworkUuidAndVariantId(networkUuid, variantToCloneId).stream()
                        .map(tombstonedEquipmentInfos -> {
                            tombstonedEquipmentInfos.setUniqueId(null);
                            tombstonedEquipmentInfos.setVariantId(variantId);
                            return tombstonedEquipmentInfos;
                        })
                        .collect(Collectors.toList())
        );
    }

    public List<EquipmentInfos> findEquipmentInfosList(List<String> equipmentIds, UUID networkUuid, String variantId) {
        return equipmentInfosRepository.findByIdInAndNetworkUuidAndVariantId(equipmentIds, networkUuid, variantId);
    }

    public void deleteAll() {
        equipmentInfosRepository.deleteAll();
        tombstonedEquipmentInfosRepository.deleteAll();
    }

    public void updateEquipment (Identifiable identifiable, UUID networkUuid, String variantId) {
        EquipmentInfos equipmentToUpdate = EquipmentInfos.builder()
            .networkUuid(networkUuid)
            .variantId(variantId)
            .id(identifiable.getId())
            .name(identifiable.getNameOrId())
            .type(identifiable.getType().name())
            .voltageLevels(EquipmentInfos.getVoltageLevelsInfos(identifiable))
            .substations(EquipmentInfos.getSubstationsInfos(identifiable))
            .build();

        equipmentInfosRepository.save(equipmentToUpdate);
    }

    public void updateLinkedEquipments (Identifiable identifiable, UUID networkUuid, String variantId) {
        if (identifiable.getType().equals(IdentifiableType.VOLTAGE_LEVEL)) {
            addAllEquipmentInfos(
                getEquipmentsFromVoltageLevelId(identifiable.getId(), networkUuid).stream()
                    .map(e -> {
                        e.setVoltageLevels(e.getVoltageLevels().stream().map(
                            vl -> vl.getId().equals(identifiable.getId()) ? new VoltageLevelInfos(vl.getId(), identifiable.getNameOrId()) : vl
                        ).collect(Collectors.toSet()));
                        return e;
                    }).toList()
            );
        } else if (identifiable.getType().equals(IdentifiableType.SUBSTATION)) {
            addAllEquipmentInfos(
                getEquipmentsFromSubstationsId(identifiable.getId(), networkUuid).stream()
                    .map(e -> {
                        e.setSubstations(e.getSubstations().stream().map(
                            s -> s.getId().equals(identifiable.getId()) ? new SubstationInfos(s.getId(), identifiable.getNameOrId()) : s
                        ).collect(Collectors.toSet()));
                        return e;
                    }).toList()
            );
        }
    }

    public List<EquipmentInfos> getEquipmentsFromVoltageLevelId(String voltageLevelId, UUID networkUuid) {
        Query searchByVoltageLevelIdQuery = QueryBuilders.nested()
            .path("voltageLevels")
            .query(QueryBuilders.bool()
                .must(QueryBuilders.match()
                    .field("voltageLevels.id")
                    .query(voltageLevelId).build()._toQuery()).build()._toQuery())
            .build()
            .query();

        Query searchByNetworkUuidQuery = QueryBuilders.term()
            .field("networkUuid.keyword")
            .value(networkUuid.toString())
            .build()._toQuery();

        Query finalQuery = QueryBuilders.bool().must(List.of(
            searchByVoltageLevelIdQuery,
            searchByNetworkUuidQuery
        )).build()._toQuery();

        return searchEquipments(finalQuery);
    }

    public List<EquipmentInfos> getEquipmentsFromSubstationsId(String substationId, UUID networkUuid) {
        Query searchBySubstationIdQuery = QueryBuilders.nested()
            .path("substations")
            .query(QueryBuilders.bool()
                .must(QueryBuilders.match()
                    .field("substations.id")
                    .query(substationId).build()._toQuery()).build()._toQuery())
            .build()
            .query();

        Query searchByNetworkUuidQuery = QueryBuilders.term()
            .field("networkUuid.keyword")
            .value(networkUuid.toString())
            .build()._toQuery();

        Query finalQuery = QueryBuilders.bool().must(List.of(
            searchBySubstationIdQuery,
            searchByNetworkUuidQuery
        )).build()._toQuery();

        return searchEquipments(finalQuery);

    }

    public List<EquipmentInfos> searchEquipments(@NonNull final Query query) {
        NativeQuery nativeQuery = new NativeQueryBuilder()
            .withQuery(query)
            .withPageable(PageRequest.of(0, PAGE_MAX_SIZE))
            .build();

        return elasticsearchOperations.search(nativeQuery, EquipmentInfos.class)
            .stream()
            .map(SearchHit::getContent)
            .toList();
    }
}
