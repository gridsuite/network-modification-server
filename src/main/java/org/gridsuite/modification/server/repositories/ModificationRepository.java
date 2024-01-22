/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.TabularModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public interface ModificationRepository extends JpaRepository<ModificationEntity, UUID> {

    // select only the columns from the base class without any left join
    //TODO This doesn't return a proper entity, it's actually just a DTO:
    //See https://docs.spring.io/spring-data/jpa/docs/current/reference/html/#projections.dtos
    //TODO can we use the simpler interface based projections instead ? To avoid repeating the columns in @Query
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.messageType, m.messageValues) FROM ModificationEntity m WHERE m.group.id = ?1 order by m.modificationsOrder")
    List<ModificationEntity> findAllBaseByGroupId(UUID uuid);

    @Query(value = "SELECT modifications_id from tabular_modification_modifications where tabular_modification_entity_id= ?1", nativeQuery = true)
    List<UUID> findSubModificationsIds(UUID uuid);

    Integer countByGroupIdAndStashed(UUID groupId, boolean stashed);
}
