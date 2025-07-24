/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.equipment.modification.LineModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@Repository
public interface LineModificationRepository extends JpaRepository<LineModificationEntity, UUID> {

    @EntityGraph(attributePaths = {"currentLimits1"}, type = EntityGraph.EntityGraphType.LOAD)
    List<LineModificationEntity> findAllLimitSets1ByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"currentLimits2"}, type = EntityGraph.EntityGraphType.LOAD)
    List<LineModificationEntity> findAllLimitSets2ByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<LineModificationEntity> findAllPropertiesByIdIn(List<UUID> ids);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM line_modification WHERE id IN ?1 ;" +
            "DELETE from current_temporary_limits_modification CL USING line_modification L where L.current_limits_modification_id1 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_temporary_limits_modification CL USING line_modification L where L.current_limits_modification_id2 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_limits_modification CL USING line_modification L where L.current_limits_modification_id1 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_limits_modification CL USING line_modification L where L.current_limits_modification_id2 = CL.id and L.id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteTabularSubModifications(List<UUID> subModificationIds, UUID tabularModificationId);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM line_modification WHERE id IN ?1 ;" +
            "DELETE from current_temporary_limits_modification CL USING line_modification L where L.current_limits_modification_id1 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_temporary_limits_modification CL USING line_modification L where L.current_limits_modification_id2 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_limits_modification CL USING line_modification L where L.current_limits_modification_id1 = CL.id and L.id IN ?1 ;" +
            "DELETE from current_limits_modification CL USING line_modification L where L.current_limits_modification_id2 = CL.id and L.id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification WHERE id = ?2 ;" +
            "DELETE FROM modification WHERE id = ?2 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteTabularModification(List<UUID> subModificationIds, UUID tabularModificationId);
}
