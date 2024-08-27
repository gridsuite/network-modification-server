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
}
