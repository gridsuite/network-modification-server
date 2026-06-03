/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.gridsuite.modification.server.entities.ModificationContainerType;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.NativeQuery;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;
import java.util.Set;
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
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.containerId = ?1 order by m.modificationsOrder")
    List<ModificationEntity> findAllBaseByContainerId(UUID uuid);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.containerId = ?1 order by m.modificationsOrder desc")
    List<ModificationEntity> findAllBaseByContainerIdReverse(UUID uuid);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.containerId = ?1 AND m.stashed = ?2 order by m.modificationsOrder")
    List<ModificationEntity> findAllByContainerId(@Param("containerId") UUID containerId, @Param("stashed") Boolean stashed);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.containerId = ?1 AND m.stashed = false AND m.activated = true AND m.id NOT IN (?2) order by m.modificationsOrder")
    List<ModificationEntity> findAllActiveModificationsByContainerId(UUID containerUuid, Set<UUID> excludedList);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type) FROM ModificationEntity m WHERE m.id IN (?1)")
    List<ModificationEntity> findMetadataIn(List<UUID> uuids);

    /**
     * @return base data of the network modifications (the data from the main common table, not those specific to each modification)
     */
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.id IN (?1) order by m.modificationsOrder")
    List<ModificationEntity> findBaseDataByIdIn(List<UUID> uuids);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder")
    List<ModificationEntity> findAllByIdIn(List<UUID> uuids);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder desc")
    List<ModificationEntity> findAllByIdInReverse(List<UUID> uuids);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modifications_modifications WHERE tabular_modifications_entity_id = :uuid", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationId(UUID uuid);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modifications_modifications WHERE tabular_modifications_entity_id = :uuid ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(UUID uuid);

    @Query("""
              SELECT m FROM ModificationEntity m
              WHERE m.containerId = :id AND m.containerType = :type
              ORDER BY m.modificationsOrder ASC
            """)
    List<ModificationEntity> findAllByContainer(@Param("id") UUID containerId,
                                                @Param("type") ModificationContainerType type);

    @Query("""
                SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description)
                  FROM ModificationEntity m
                 WHERE m.containerId IN :ids AND m.containerType = :type
                 ORDER BY m.containerId, m.modificationsOrder ASC
            """)
    List<ModificationEntity> findAllByContainers(@Param("ids") Collection<UUID> containerIds,
                                                 @Param("type") ModificationContainerType type);

    @Query("""
      SELECT m FROM ModificationEntity m
      WHERE m.containerId = :id AND m.containerType = :type AND m.stashed = false
      ORDER BY m.modificationsOrder ASC
    """)
    List<ModificationEntity> findActiveByContainer(@Param("id") UUID containerId,
                                                   @Param("type") ModificationContainerType type);

    @Query("""
              SELECT COUNT(m) FROM ModificationEntity m
              WHERE m.containerId = :id AND m.containerType = :type AND m.stashed = :stashed
            """)
    int countByContainerAndStashed(@Param("id") UUID containerId,
                                   @Param("type") ModificationContainerType type,
                                   @Param("stashed") boolean stashed);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM line_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findLineModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM two_windings_transformer_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findTwtModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(current_limits_id AS VARCHAR) FROM operational_limits_group_modification WHERE uuid IN ?1", nativeQuery = true)
    List<UUID> findCurrentLimitsIdsByOpLimitsGroupsIds(List<UUID> uuids);

    void deleteAllByIdIn(List<UUID> ids);

    @Query("SELECT c.id FROM CompositeModificationEntity c WHERE c.id IN :ids")
    Set<UUID> findExistingCompositeModificationIds(@Param("ids") List<UUID> ids);

    /**
     * Recursively returns all <em>composite</em> descendants of {@code compositeUuid}
     * (i.e. only the composites in the subtree, leaves excluded).
     */
    @NativeQuery("""
            WITH RECURSIVE descendants(id) AS (
                SELECT m.id
                  FROM modification m
                 WHERE m.container_id = :compositeUuid
                   AND m.container_type = 'COMPOSITE'
                UNION ALL
                SELECT m.id
                  FROM modification m
                  JOIN descendants d ON m.container_id = d.id
                 WHERE m.container_type = 'COMPOSITE'
            )
            SELECT CAST(c.id AS VARCHAR)
              FROM composite_modification c
             WHERE c.id IN (SELECT id FROM descendants)
            """)
    List<UUID> findOnlyCompositeChildrenUuids(@Param("compositeUuid") UUID compositeUuid);

    /**
     * Returns the composite UUID followed by every descendant UUID (composites <em>and</em> leaves),
     * ordered depth-first by {@code modifications_order} at each level.
     */
    @NativeQuery("""
            WITH RECURSIVE hierarchy(id, path) AS (
                SELECT CAST(:compositeUuid AS uuid), ARRAY[0]
                UNION ALL
                SELECT m.id, h.path || m.modifications_order
                  FROM modification m
                  JOIN hierarchy h ON m.container_id = h.id
                 WHERE m.container_type = 'COMPOSITE'
            )
            SELECT CAST(id AS VARCHAR) FROM hierarchy ORDER BY path
            """)
    List<UUID> findAllChildrenUuids(@Param("compositeUuid") UUID compositeUuid);

    interface CompositeDepth {
        String getId();

        Integer getDepth();
    }

    /**
     * For each root composite in {@code compositeUuids}, returns the maximum depth of its
     * (unstashed) descendant tree. Composites with no unstashed children do not appear in the result.
     */
    @NativeQuery("""
            WITH RECURSIVE hierarchy(root_id, id, level) AS (
                SELECT m.container_id, m.id, 1
                  FROM modification m
                 WHERE m.container_id IN (:compositeUuids)
                   AND m.container_type = 'COMPOSITE'
                   AND m.stashed = false
                UNION ALL
                SELECT h.root_id, m.id, h.level + 1
                  FROM modification m
                  JOIN hierarchy h ON m.container_id = h.id
                 WHERE m.container_type = 'COMPOSITE'
                   AND m.stashed = false
            )
            SELECT CAST(root_id AS VARCHAR) AS id, MAX(level) AS depth
              FROM hierarchy
             GROUP BY root_id
            """)
    List<CompositeDepth> getCompositesMaxDepth(@Param("compositeUuids") List<UUID> compositeUuids);

    @EntityGraph(attributePaths = {"modifications"}, type = EntityGraph.EntityGraphType.LOAD)
    List<CompositeModificationEntity> findAllCompositesWithModificationsByIdIn(List<UUID> compositeUuids);
}
