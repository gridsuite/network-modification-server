/*
  Copyright (c) 2025, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

/**
 * @author Kevin Le Saulnier <kevin.lesaulnier at rte-france.com>
 */
public interface ModificationApplicationRepository extends JpaRepository<ModificationApplicationEntity, UUID> {
    void deleteAllByNetworkUuidAndModificationGroupIdIn(UUID networkUuid, List<UUID> groupUuid);

    void deleteAllByModificationGroupIdIn(List<UUID> groupUuid);

    void deleteAllByModificationIdIn(List<UUID> modificationIds);

    @EntityGraph(attributePaths = {"modification", "modification.group"}, type = EntityGraph.EntityGraphType.LOAD)
    List<ModificationApplicationEntity> findWithModificationAndGroupByNetworkUuid(UUID networkUuid);

    @Query("SELECT distinct ma.networkUuid from ModificationApplicationEntity ma")
    List<UUID> findDistinctNetworkUuids();
}
