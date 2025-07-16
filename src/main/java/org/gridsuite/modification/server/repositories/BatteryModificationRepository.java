/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.equipment.modification.BatteryModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Repository
public interface BatteryModificationRepository extends JpaRepository<BatteryModificationEntity, UUID> {

    @EntityGraph(attributePaths = {"reactiveCapabilityCurvePoints"}, type = EntityGraph.EntityGraphType.LOAD)
    List<BatteryModificationEntity> findAllReactiveCapabilityCurvePointsByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<BatteryModificationEntity> findAllPropertiesByIdIn(List<UUID> ids);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM battery_modification_entity_reactive_capability_curve_points cp WHERE cp.battery_modification_entity_id IN ?1 ;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM battery_modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteTabularSubModifications(List<UUID> subModificationIds, UUID tabularModificationId);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM battery_modification_entity_reactive_capability_curve_points cp WHERE cp.battery_modification_entity_id IN ?1 ;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM battery_modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification WHERE id = ?2 ;" +
            "DELETE FROM modification WHERE id = ?2 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteTabularModification(List<UUID> subModificationIds, UUID tabularModificationId);
}
