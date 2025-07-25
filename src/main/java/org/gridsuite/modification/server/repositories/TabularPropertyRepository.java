/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.TabularPropertyEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.util.UUID;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */

public interface TabularPropertyRepository extends JpaRepository<TabularPropertyEntity, UUID> {
    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM tabular_property tp WHERE tp.tabular_modification_id = ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteTabularProperties(UUID tabularModificationId);
}
