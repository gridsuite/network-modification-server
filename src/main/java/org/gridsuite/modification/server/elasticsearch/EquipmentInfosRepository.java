/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.EquipmentInfos;
import org.springframework.boot.autoconfigure.condition.ConditionalOnExpression;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.lang.NonNull;

import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@ConditionalOnExpression("'${spring.data.elasticsearch.enabled:false}' == 'true'")
@Lazy
public interface EquipmentInfosRepository extends ElasticsearchRepository<EquipmentInfos, String> {
    Iterable<EquipmentInfos> findAllByNetworkUuid(@NonNull UUID networkUuid);

    Iterable<EquipmentInfos> findAllByNetworkUuidAndVariantId(@NonNull UUID networkUuid, @NonNull String variantId);

    void deleteByIdAndNetworkUuidAndVariantIdAndTombstoned(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId, Boolean tombstoned);

    void deleteAllByNetworkUuidAndVariantId(UUID networkUuid, String variantId);

    Iterable<EquipmentInfos> findByIdAndNetworkUuidAndVariantIdAndTombstoned(@NonNull String equipmentId, @NonNull UUID networkUuid, @NonNull String variantId, Boolean tombstoned);
}
