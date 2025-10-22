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

    @EntityGraph(attributePaths = {"operationalLimitsGroups"}, type = EntityGraph.EntityGraphType.LOAD)
    List<LineModificationEntity> findAllOperationalLimitsGroupsByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<LineModificationEntity> findAllPropertiesByIdIn(List<UUID> ids);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM line_modification_operational_limits_groups lm WHERE lm.branch_id IN ?3 ;" +
            "DELETE FROM operational_limits_group_modification ol WHERE ol.uuid IN ?2 ;" +
            "DELETE FROM current_temporary_limits_modification cl WHERE cl.id IN ?1 ;" +
            "DELETE FROM current_limits_modification cl WHERE cl.id IN ?1 ;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?3 ;" +
            "DELETE FROM line_modification WHERE id IN ?3 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteSomeTabularSubModifications(List<UUID> currentLimitsIds, List<UUID> opLimitsGroupsIds, List<UUID> subModificationIdsPart);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM tabular_modifications_modifications WHERE tabular_modifications_entity_id = ?1 ;" +
            "DELETE FROM modification WHERE id IN ?2 ;" +
            "COMMIT;", nativeQuery = true)
    // This function is generic and can work on any tabular modification/creation
    void deleteTabularModificationModifications(UUID tabularModificationId, List<UUID> subModificationIds);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM tabular_modifications WHERE id = ?1 ;" +
            "DELETE FROM modification WHERE id = ?1 ;" +
            "COMMIT;", nativeQuery = true)
    // This function is generic and can work on any tabular modification/creation
    void deleteTabularModificationItself(UUID tabularModificationId);
}
