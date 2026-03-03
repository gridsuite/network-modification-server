/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import org.gridsuite.modification.server.entities.catalog.LineTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
@Repository
public interface LineTypesCatalogRepository extends JpaRepository<LineTypeEntity, UUID> {

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM temporary_limit_for_line_catalog", nativeQuery = true)
    void deleteAllTemporaryLimits();

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM limits_for_line_type", nativeQuery = true)
    void deleteAllLimitsForLineType();

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM aerial_line_types_catalog", nativeQuery = true)
    void deleteAllAerialLineTypes();

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM underground_line_types_catalog", nativeQuery = true)
    void deleteAllUndergroundLineTypes();

    @Transactional
    @Modifying
    @Query(value = "DELETE FROM line_types_catalog", nativeQuery = true)
    void deleteAllLineTypes();
}
