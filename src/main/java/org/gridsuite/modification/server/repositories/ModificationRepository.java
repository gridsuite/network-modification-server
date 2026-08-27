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
import org.springframework.data.jpa.repository.Modifying;
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
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) "
            + "FROM ModificationEntity m WHERE m.container.id = ?1 order by m.modificationsOrder")
    List<ModificationEntity> findAllBaseByContainerId(UUID containerId);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) "
            + "FROM ModificationEntity m WHERE m.container.id = ?1 order by m.modificationsOrder desc")
    List<ModificationEntity> findAllBaseByContainerIdReverse(UUID containerId);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.container.id = ?1 AND m.stashed = ?2 order by m.modificationsOrder")
    List<ModificationEntity> findAllByContainerId(@Param("containerId") UUID containerId, @Param("stashed") Boolean stashed);

    /**
     * @return the modifications of the container that are applied on the given root network tag, that is the activated
     * ones the tag does not deactivate. A reference is resolved to the shared modification carrying the applicability.
     */
    @Query("""
            SELECT m FROM ModificationEntity m
            WHERE m.container.id = :containerId AND m.stashed = false AND m.activated = true
              AND (:rootNetworkTag IS NULL OR NOT EXISTS (
                  SELECT 1 FROM ModificationEntity holder JOIN holder.applicabilityByRootNetworkTag a
                  WHERE holder.id = COALESCE((SELECT r.referenceId FROM ModificationReferenceEntity r WHERE r.id = m.id), m.id)
                    AND KEY(a) = :rootNetworkTag AND VALUE(a) = false))
            ORDER BY m.modificationsOrder
            """)
    List<ModificationEntity> findAllActiveModificationsByContainerId(@Param("containerId") UUID containerId,
                                                                    @Param("rootNetworkTag") String rootNetworkTag);

    @Query(value = "SELECT new ModificationEntity(m.id, m.type) FROM ModificationEntity m WHERE m.id IN (?1)")
    List<ModificationEntity> findMetadataIn(List<UUID> uuids);

    /**
     * @return base data of the network modifications (the data from the main common table, not those specific to each modification)
     */
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) FROM ModificationEntity m WHERE m.id IN (?1) order by "
            + "m.modificationsOrder")
    List<ModificationEntity> findBaseDataByIdIn(List<UUID> uuids);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder")
    List<ModificationEntity> findAllByIdIn(List<UUID> uuids);

    /**
     * @return one row per applicability entry: the modification id, the root network tag and whether it is applicable.
     * A reference has no applicability of its own, so it is resolved to the shared modification it points to.
     */
    @Query("""
            SELECT m.id, KEY(a), VALUE(a)
            FROM ModificationEntity m, ModificationEntity holder
            JOIN holder.applicabilityByRootNetworkTag a
            WHERE m.id IN (:uuids)
              AND holder.id = COALESCE((SELECT r.referenceId FROM ModificationReferenceEntity r WHERE r.id = m.id), m.id)
            """)
    List<Object[]> findApplicabilitiesByIdIn(@Param("uuids") Collection<UUID> uuids);

    /**
     * @return every modification held by the given containers, the content of their composites included.
     */
    @NativeQuery("""
        WITH RECURSIVE descendants(id) AS (
            SELECT m.id FROM modification m WHERE m.container_id IN (:containerIds)
            UNION ALL
            SELECT m.id FROM modification m JOIN descendants d ON m.container_id = d.id
        )
        SELECT CAST(id AS VARCHAR) FROM descendants
        """)
    List<UUID> findAllDescendantModificationIdsByContainerIds(@Param("containerIds") Collection<UUID> containerIds);

    /**
     * @return the shared modifications the references among {@code ids} point to, deduplicated: several references
     * may well point to the same one.
     */
    @Query("SELECT DISTINCT r.referenceId FROM ModificationReferenceEntity r WHERE r.id IN :ids")
    List<UUID> findReferencedModificationIds(@Param("ids") Collection<UUID> ids);

    /**
     * Copies the applicability of {@code fromTag} to {@code toTag}, skipping the modifications that already have an
     * entry for {@code toTag}.
     */
    @Modifying
    @NativeQuery("""
        INSERT INTO modification_root_network_applicability (modification_id, root_network_tag, applicable)
        SELECT a.modification_id, :toTag, a.applicable
          FROM modification_root_network_applicability a
         WHERE a.modification_id IN (:ids) AND a.root_network_tag = :fromTag
           AND NOT EXISTS (SELECT 1 FROM modification_root_network_applicability b
                            WHERE b.modification_id = a.modification_id AND b.root_network_tag = :toTag)
        """)
    void copyRootNetworkApplicability(@Param("ids") Collection<UUID> ids, @Param("fromTag") String fromTag, @Param("toTag") String toTag);

    /**
     * Deletes the {@code toTag} entries of the given modifications, restricted to those also holding a
     * {@code fromTag} entry. Useful to prepare renaming with {@link #renameRootNetworkApplicability}.
     */
    @Modifying
    @NativeQuery("""
        DELETE FROM modification_root_network_applicability a
         WHERE a.modification_id IN (:ids) AND a.root_network_tag = :toTag
           AND EXISTS (SELECT 1 FROM modification_root_network_applicability b
                        WHERE b.modification_id = a.modification_id AND b.root_network_tag = :fromTag)
        """)
    void deleteRootNetworkApplicabilitiesTakenOverBy(@Param("ids") Collection<UUID> ids, @Param("fromTag") String fromTag, @Param("toTag") String toTag);

    @Modifying
    @NativeQuery("""
        UPDATE modification_root_network_applicability SET root_network_tag = :toTag
         WHERE modification_id IN (:ids) AND root_network_tag = :fromTag
        """)
    void renameRootNetworkApplicability(@Param("ids") Collection<UUID> ids, @Param("fromTag") String fromTag, @Param("toTag") String toTag);

    @Modifying
    @NativeQuery("DELETE FROM modification_root_network_applicability WHERE modification_id IN (:ids) AND root_network_tag IN (:tags)")
    void deleteRootNetworkApplicabilities(@Param("ids") Collection<UUID> ids, @Param("tags") Collection<String> tags);

    @Query(value = "SELECT m FROM ModificationEntity m WHERE m.id IN (?1) ORDER BY m.modificationsOrder desc")
    List<ModificationEntity> findAllByIdInReverse(List<UUID> uuids);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modifications_modifications WHERE tabular_modifications_entity_id = :uuid", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationId(UUID uuid);

    @Query(value = "SELECT cast(modifications_id AS VARCHAR) FROM tabular_modifications_modifications WHERE tabular_modifications_entity_id = :uuid ORDER BY modifications_order", nativeQuery = true)
    List<UUID> findSubModificationIdsByTabularModificationIdOrderByModificationsOrder(UUID uuid);

    // children of one / many containers (no type param — id disambiguates)
    @Query("SELECT m FROM ModificationEntity m WHERE m.container.id = :containerId ORDER BY m.modificationsOrder ASC")
    List<ModificationEntity> findAllByContainer(@Param("containerId") UUID containerId);

    @Query("""
          SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description)
            FROM ModificationEntity m
           WHERE m.container.id IN :containerIds
           ORDER BY m.container.id, m.modificationsOrder ASC
        """)
    List<ModificationEntity> findAllByContainers(@Param("containerIds") Collection<UUID> containerIds);

    @Query(value = """
            SELECT CAST(m.container_id AS VARCHAR)
              FROM modification m
              JOIN modification_container c ON c.type = 'COMPOSITE' AND c.id = m.container_id
             WHERE m.id = :uuid
            """, nativeQuery = true)
    UUID findCompositeContainerIdByModificationId(@Param("uuid") UUID uuid);

    /**
     * @return one [modification id, composite container id] row per modification actually nested in a composite;
     * modifications sitting directly under a group have no row
     */
    @Query(value = """
            SELECT CAST(m.id AS VARCHAR), CAST(m.container_id AS VARCHAR)
              FROM modification m
              JOIN modification_container c ON c.type = 'COMPOSITE' AND c.id = m.container_id
             WHERE m.id IN :uuids
            """, nativeQuery = true)
    List<Object[]> findCompositeContainerIdsByModificationIds(@Param("uuids") Collection<UUID> uuids);

    @Query("""
          SELECT COUNT(m) FROM ModificationEntity m
          WHERE m.container.id = :containerId AND m.stashed = :stashed
        """)
    int countByContainerAndStashed(@Param("containerId") UUID containerId, @Param("stashed") boolean stashed);

    // return the referenced modification of a modification reference
    @Query(value = "SELECT new ModificationEntity(m.id, m.type, m.date, m.stashed, m.activated, m.messageType, m.messageValues, m.description) " +
            "from ModificationEntity m WHERE m.id = (select r.referenceId from ModificationReferenceEntity r WHERE r.id = ?1)")
    ModificationEntity findReferencedModificationMetadataByReferenceId(UUID uuid);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM line_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findLineModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(operational_limits_groups_id AS VARCHAR) FROM two_windings_transformer_modification_operational_limits_groups WHERE branch_id IN ?1", nativeQuery = true)
    List<UUID> findTwtModificationOpLimitsGroupsIdsByBranchIds(List<UUID> uuids);

    @Query(value = "SELECT cast(current_limits_id AS VARCHAR) FROM operational_limits_group_modification WHERE uuid IN ?1", nativeQuery = true)
    List<UUID> findCurrentLimitsIdsByOpLimitsGroupsIds(List<UUID> uuids);

    void deleteAllByIdIn(List<UUID> ids);

    // still just a PK existence probe, but on the leaf table
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
            UNION ALL
            SELECT m.id
              FROM modification m
              JOIN descendants d ON m.container_id = d.id
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
               AND m.stashed = false
            UNION ALL
            SELECT h.root_id, m.id, h.level + 1
              FROM modification m
              JOIN hierarchy h ON m.container_id = h.id
             WHERE m.stashed = false
        )
        SELECT CAST(root_id AS VARCHAR) AS id, MAX(level) AS depth
          FROM hierarchy
         GROUP BY root_id
        """)
    List<CompositeDepth> getCompositesMaxDepth(@Param("compositeUuids") List<UUID> compositeUuids);

    @EntityGraph(attributePaths = {"content.modifications"}, type = EntityGraph.EntityGraphType.LOAD)
    List<CompositeModificationEntity> findAllCompositesWithModificationsByIdIn(List<UUID> compositeUuids);
}
