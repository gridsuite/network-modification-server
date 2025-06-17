/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.elasticsearch;

import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;

import java.util.List;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
public interface ModificationApplicationInfosRepository extends ElasticsearchRepository<ModificationApplicationInfos, String> {
    void deleteAllByNetworkUuidAndGroupUuidIn(UUID networkUuid, List<UUID> groupUuid);

    void deleteAllByGroupUuidIn(List<UUID> groupUuid);

    void deleteAllByModificationUuidIn(List<UUID> modificationUuid);

    void deleteAllByNetworkUuid(UUID networkUuid);
}
