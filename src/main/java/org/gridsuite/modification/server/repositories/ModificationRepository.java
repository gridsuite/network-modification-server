/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public interface ModificationRepository extends JpaRepository<ModificationEntity, UUID> {

    // select only the columns from the base class without any left join
    //TODO This doesn't return a proper entity, it's actually just a DTO:
    //See https://docs.spring.io/spring-data/jpa/docs/current/reference/html/#projections.dtos
    //TODO can we use the simpler interface based projections instead ? To avoid repeating the columns in @Query
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues) FROM ModificationEntity m WHERE m.group.id = ?1 order by m.modificationsOrder")
    List<ModificationEntity> findAllBaseByGroupId(UUID uuid);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues) FROM ModificationEntity m WHERE m.group.id = ?1 order by m.modificationsOrder desc")
    List<ModificationEntity> findAllBaseByGroupIdReverse(UUID uuid);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.group.id = ?1 AND m.stashed = ?2 order by m.modificationsOrder")
    List<ModificationEntity> findAllByGroupId(@Param("groupId") UUID groupId, @Param("stashed") Boolean stashed);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type) FROM ModificationEntity m WHERE m.id IN (?1)")
    List<ModificationEntity> findMetadataIn(List<UUID> uuids);

    /**
     * @return base data of the network modifications (the data from the main common table, not those specific to each modification)
     */
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues) FROM ModificationEntity m WHERE m.id IN (?1) order by m.modificationsOrder")
    List<ModificationEntity> findBaseDataByIdIn(List<UUID> uuids);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder")
    List<ModificationEntity> findAllByIdIn(List<UUID> uuids);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder desc")
    List<ModificationEntity> findAllByIdInReverse(List<UUID> uuids);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modification_modifications WHERE tabular_modification_entity_id = :uuid ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(UUID uuid);

    @Query(value = "SELECT cast(creations_id AS VARCHAR) FROM tabular_creation_creations WHERE tabular_creation_entity_id = :uuid ORDER BY creations_order", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularCreationIdOrderByModificationsOrder(UUID uuid);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modification_modifications WHERE tabular_modification_entity_id = :uuid", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationId(UUID uuid);

    @Query(value = "SELECT cast(modification_id AS VARCHAR) FROM composite_modification_sub_modifications WHERE id = :uuid ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findModificationIdsByCompositeModificationId(UUID uuid);

    Integer countByGroupIdAndStashed(UUID groupId, boolean stashed);

    void deleteAllByIdIn(List<UUID> ids);
}
