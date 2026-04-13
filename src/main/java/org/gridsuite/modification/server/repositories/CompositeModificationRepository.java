/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.UUID;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */

public interface CompositeModificationRepository extends JpaRepository<CompositeModificationEntity, UUID> {

    @Modifying
    @Query(value = """
        DELETE FROM composite_modification_sub_modifications
        WHERE id = :compositeId
        """, nativeQuery = true)
    void deleteCompositeSubModifications(@Param("compositeId") UUID compositeId);

    @Modifying
    @Query(value = """
        INSERT INTO composite_modification_sub_modifications (id, modification_id, modifications_order)
        VALUES (:compositeId, :modificationId, :position)
        """, nativeQuery = true)
    void insertCompositeSubModification(
            @Param("compositeId") UUID compositeId,
            @Param("modificationId") UUID modificationId,
            @Param("position") int position);
}
