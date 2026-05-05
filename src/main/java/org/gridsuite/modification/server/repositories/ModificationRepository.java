/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.NativeQuery;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

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
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.group.id = ?1 order by m.modificationsOrder")
    List<ModificationEntity> findAllBaseByGroupId(UUID uuid);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.group.id = ?1 order by m.modificationsOrder desc")
    List<ModificationEntity> findAllBaseByGroupIdReverse(UUID uuid);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.group.id = ?1 AND m.stashed = ?2 order by m.modificationsOrder")
    List<ModificationEntity> findAllByGroupId(@Param("groupId") UUID groupId, @Param("stashed") Boolean stashed);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.group.id = ?1 AND m.stashed = false AND m.activated = true AND m.id NOT IN (?2) order by m.modificationsOrder")
    List<ModificationEntity> findAllActiveModificationsByGroupId(UUID groupUuid, Set<UUID> excludedList);

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

    @Query(value = "SELECT cast(modification_id AS VARCHAR) FROM composite_modification_sub_modifications WHERE id = :uuid ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findModificationIdsByCompositeModificationId(UUID uuid);

    // return the uuid of the composite containing the modifcation sent as parameter
    // TODO : à changer après fusion de ma fiche sur les ordre de modifications de réseau
    @Query(value = "SELECT cast(id AS UUID) FROM composite_modification_sub_modifications WHERE modification_id = :uuid ORDER BY modifications_order", nativeQuery = true)
    UUID findCompositeIdByContainedModificationId(UUID uuid);

    @Query(value = "SELECT cast(modification_id AS VARCHAR) FROM composite_modification_sub_modifications WHERE id IN (?1) ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findModificationIdsByCompositeModificationIdIn(List<UUID> uuids);

    Integer countByGroupIdAndStashed(UUID groupId, boolean stashed);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM line_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findLineModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM two_windings_transformer_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findTwtModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(current_limits_id AS VARCHAR) FROM operational_limits_group_modification WHERE uuid IN ?1", nativeQuery = true)
    List<UUID> findCurrentLimitsIdsByOpLimitsGroupsIds(List<UUID> uuids);

    void deleteAllByIdIn(List<UUID> ids);

    @Query(value = "SELECT DISTINCT cast(id AS VARCHAR) FROM composite_modification_sub_modifications WHERE id IN (?1)", nativeQuery = true)
    Set<UUID> findExistingCompositeModificationIds(List<UUID> compositeIds);

    @NativeQuery("WITH RECURSIVE ModificationHierarchy (id) AS ( " +
        "  SELECT m0.id" +
        "  FROM composite_modification_sub_modifications m0 " +
        "  WHERE m0.id = :compositeUuid " +
        "  UNION ALL " +
        "  SELECT distinct m.modification_id" +
        "  FROM composite_modification_sub_modifications m " +
        "  INNER JOIN ModificationHierarchy mh ON m.id = mh.id " +
        ") " +
        "SELECT cast(m.id AS VARCHAR) FROM composite_modification m " +
        "WHERE m.id IN (SELECT mh.id FROM ModificationHierarchy mh)")
    List<UUID> findOnlyCompositeChildrenUuids(UUID compositeUuid);

    @NativeQuery("WITH RECURSIVE ModificationHierarchy (id) AS ( " +
        "  SELECT m0.id" +
        "  FROM composite_modification_sub_modifications m0 " +
        "  WHERE m0.id = :compositeUuid " +
        "  UNION ALL " +
        "  SELECT distinct m.modification_id" +
        "  FROM composite_modification_sub_modifications m " +
        "  INNER JOIN ModificationHierarchy mh ON m.id = mh.id " +
        ") " +
        "SELECT distinct cast(m.id AS VARCHAR) FROM ModificationHierarchy m ")
    List<UUID> findAllChildrenUuids(UUID compositeUuid);

    interface CompositeDepth {
        String getId();

        Integer getDepth();
    }

    @NativeQuery("WITH RECURSIVE ModificationHierarchy (root_id, id, level) AS ( " +
            "    SELECT m0.id, m0.modification_id, 1 " +
            "    FROM composite_modification_sub_modifications m0 " +
            "    INNER JOIN modification mod ON mod.id = m0.modification_id AND mod.stashed = false " +
            "    WHERE m0.id IN (:compositeUuids) " +
            "    UNION ALL " +
            "    SELECT mh.root_id, m.modification_id, mh.level + 1 " +
            "    FROM composite_modification_sub_modifications m " +
            "    INNER JOIN modification mod ON mod.id = m.modification_id AND mod.stashed = false " +
            "    INNER JOIN ModificationHierarchy mh ON m.id = mh.id " +
            ") " +
            "SELECT cast(root_id AS VARCHAR) AS id, MAX(level) AS depth FROM ModificationHierarchy GROUP BY root_id")
    List<CompositeDepth> getCompositesMaxDepth(@Param("compositeUuids") List<UUID> compositeUuids);

    @EntityGraph(attributePaths = {"modifications"}, type = EntityGraph.EntityGraphType.LOAD)
    List<CompositeModificationEntity> findAllCompositesWithModificationsByIdIn(List<UUID> compositeUuids);

    @NativeQuery("WITH RECURSIVE Ancestors (id, parent_id) AS ( " +
            "    SELECT :modificationUuid, cm.id " +
            "    FROM composite_modification_sub_modifications cm " +
            "    WHERE cm.modification_id = :modificationUuid " +
            "    UNION ALL " +
            "    SELECT a.parent_id, cm.id " +
            "    FROM composite_modification_sub_modifications cm " +
            "    INNER JOIN Ancestors a ON cm.modification_id = a.parent_id " +
            ") " +
            "SELECT cast(COALESCE(" +
            "    (SELECT a.parent_id FROM Ancestors a WHERE a.parent_id NOT IN (SELECT modification_id FROM composite_modification_sub_modifications) LIMIT 1)," +
            "    (SELECT a.id FROM Ancestors a LIMIT 1)," +
            "    :modificationUuid" +
            ") AS VARCHAR)")
    String findRootAncestorUuid(@Param("modificationUuid") UUID modificationUuid);
}
