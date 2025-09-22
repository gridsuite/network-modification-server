/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.equipment.modification.ShuntCompensatorModificationEntity;
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
public interface ShuntCompensatorModificationRepository extends JpaRepository<ShuntCompensatorModificationEntity, UUID> {

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<ShuntCompensatorModificationEntity> findAllPropertiesByIdIn(List<UUID> ids);

    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM free_property fp WHERE fp.equipment_modification_id IN ?1 ;" +
            "DELETE FROM shunt_compensator_modification WHERE id IN ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteSomeTabularSubModifications(List<UUID> subModificationIdsPart);
}
