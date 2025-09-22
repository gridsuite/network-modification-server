/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.UUID;

/**
 * @author Seddik Yengui <seddik.yengui_externe at rte-france.com>
 */

public interface GeneratorCreationRepository extends JpaRepository<GeneratorCreationEntity, UUID> {
    @EntityGraph(attributePaths = {"reactiveCapabilityCurvePoints"}, type = EntityGraph.EntityGraphType.LOAD)
    List<GeneratorCreationEntity> findAllReactiveCapabilityCurvePointsByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<GeneratorCreationEntity> findAllPropertiesByIdIn(List<UUID> ids);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM generator_creation_entity_reactive_capability_curve_points cp WHERE cp.generator_creation_entity_id IN ?1 ;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM generator_creation WHERE id IN ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteSomeTabularSubModifications(List<UUID> subModificationIdsPart);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM tabular_creation_creations WHERE tabular_creation_entity_id = ?1 ;" +
            "DELETE FROM modification WHERE id IN ?2 ;" +
            "COMMIT;", nativeQuery = true)
    // This function is generic and can work on any creation
    void deleteTabularCreationCreations(UUID tabularModificationId, List<UUID> subModificationIds);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM tabular_creation WHERE id = ?1 ;" +
            "DELETE FROM modification WHERE id = ?1 ;" +
            "COMMIT;", nativeQuery = true)
    // This function is generic and can work on any creation
    void deleteTabularCreationItself(UUID tabularModificationId);
}
