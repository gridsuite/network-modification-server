/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.server.entities.CompositeModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.UUID;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@Repository
public interface CompositeModificationRepository extends JpaRepository<CompositeModificationEntity, UUID> {

    @SneakyThrows
    default void renameCompositeModifications(CompositeModificationEntity compositeEntity, CompositeModificationInfos compositeMetadata) {
        compositeEntity.setName(compositeMetadata.getName());
        compositeEntity.setMessageValues(new ObjectMapper().writeValueAsString(compositeMetadata.getMapMessageValues()));
    }

    /**
     * Removes a sub-modification from a composite's join table directly via a native DELETE,
     * bypassing JPA collection management entirely.
     * This is required when stashing a sub-modification: if we removed the entity from the
     * composite's in-memory collection and then called setModifications(), JPA's orphanRemoval
     * would delete the sub-modification row before we can flip its stashed flag to true.
     * A direct SQL DELETE on the join table avoids that cascade, leaving the modification
     * entity intact so it can be reassigned to the parent group and stashed normally.
     */
    @Modifying
    @Query(value = """
            DELETE FROM composite_modification_sub_modifications
            WHERE id = :compositeUuid
              AND modification_id = :subModificationUuid
            """, nativeQuery = true)
    void removeSubModification(@Param("compositeUuid") UUID compositeUuid,
                               @Param("subModificationUuid") UUID subModificationUuid);
}
