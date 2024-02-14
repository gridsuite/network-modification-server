/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
@Repository
public interface GeneratorModificationRepository extends JpaRepository<GeneratorModificationEntity, UUID> {

    @EntityGraph(attributePaths = {"reactiveCapabilityCurvePoints"}, type = EntityGraph.EntityGraphType.LOAD)
    List<GeneratorModificationEntity> findAllReactiveCapabilityCurvePointsByIdIn(List<UUID> ids);

    @EntityGraph(attributePaths = {"properties"}, type = EntityGraph.EntityGraphType.LOAD)
    List<GeneratorModificationEntity> findAllPropertiesByIdIn(List<UUID> ids);
}