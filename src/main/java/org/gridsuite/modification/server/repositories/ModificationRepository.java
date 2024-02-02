/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import java.util.List;
import java.util.UUID;

import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
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

    @Query(value = "SELECT cast(modifications_id as varchar) from tabular_modification_modifications where tabular_modification_entity_id= ?1", nativeQuery = true)
    List<UUID> findSubModificationsIds(UUID uuid);

    Integer countByGroupIdAndStashed(UUID groupId, boolean stashed);

    @Modifying
    @Query(value = "BEGIN;" +
            "ALTER TABLE modification DISABLE TRIGGER ALL;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "ALTER TABLE modification ENABLE TRIGGER ALL;" +
            "COMMIT;", nativeQuery = true)
    void deleteModificationByIds(List<UUID> ids);

    @Modifying
    @Query(value = "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?1", nativeQuery = true)
    void deleteTabularModificationInJoinTableByIds(UUID id);

//    @Modifying
//    @Query(value = "ALTER TABLE modification DISABLE TRIGGER ALL;", nativeQuery = true)
//    void disableTrigger();
//
//    @Modifying
//    @Query(value = "ALTER TABLE modification ENABLE TRIGGER ALL;", nativeQuery = true)
//    void enableTrigger();

}
