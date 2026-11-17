/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import com.google.common.collect.Lists;
import org.gridsuite.modification.server.dto.elasticsearch.BasicEquipmentInfos;
import org.gridsuite.modification.server.dto.elasticsearch.EquipmentInfos;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.lang.NonNull;

import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
public interface EquipmentInfosRepository extends ElasticsearchRepository<EquipmentInfos, String> {
    List<EquipmentInfos> findAllByNetworkUuidAndVariantId(@NonNull UUID networkUuid, @NonNull String variantId);

    default List<EquipmentInfos> findByIdInAndNetworkUuidAndVariantId(@NonNull List<String> equipmentIds, @NonNull UUID networkUuid, @NonNull String variantId) {
        return Lists.newArrayList(findAllById(toUniqueIds(equipmentIds, networkUuid, variantId)));
    }

    void deleteAllByNetworkUuidAndVariantId(UUID networkUuid, String variantId);

    default void deleteByIdInAndNetworkUuidAndVariantId(@NonNull List<String> equipmentIds, @NonNull UUID networkUuid, @NonNull String variantId) {
        deleteAllById(toUniqueIds(equipmentIds, networkUuid, variantId));
    }

    private static List<String> toUniqueIds(List<String> equipmentIds, UUID networkUuid, String variantId) {
        return equipmentIds.stream().map(id -> BasicEquipmentInfos.buildUniqueId(networkUuid, variantId, id)).toList();
    }
}
