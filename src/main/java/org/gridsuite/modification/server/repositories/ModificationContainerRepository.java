/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.AbstractModificationContainerEntity;
import org.gridsuite.modification.server.entities.ModificationContainerType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import java.util.UUID;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

public interface ModificationContainerRepository extends JpaRepository<AbstractModificationContainerEntity, UUID> {

    @Query(value = """
          SELECT container_type FROM modification_container m
          WHERE id = :id
        """, nativeQuery = true)
    ModificationContainerType getContainerTypeById(UUID id);

}
