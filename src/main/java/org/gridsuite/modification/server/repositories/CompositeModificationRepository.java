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
}
