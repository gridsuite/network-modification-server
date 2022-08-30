/*
  Copyright (c) 2021, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories;

import java.util.List;
import java.util.UUID;

import org.gridsuite.modification.server.entities.ModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 */
@Repository
public interface ModificationRepository extends JpaRepository<ModificationEntity, UUID> {
    @Query(value = "SELECT *, 0 AS clazz_ FROM modification WHERE group_id = ?1 order by modifications_order", nativeQuery = true)
    List<ModificationEntity> findAllBaseByGroupId(UUID uuid);

    @Query(value = "SELECT DISTINCT l FROM LineCreationEntity l left join fetch l.currentLimits1 left join fetch l.currentLimits2 WHERE l.id = ?1")
    ModificationEntity findLineCreationById(UUID uuid);

    @Query(value = "SELECT DISTINCT t FROM TwoWindingsTransformerCreationEntity t left join fetch t.currentLimits1 left join fetch t.currentLimits2 WHERE t.id = ?1")
    ModificationEntity find2wtCreationById(UUID uuid);

    @Query(value = "SELECT DISTINCT line FROM LineAttachToVoltageLevelEntity line left join fetch line.lineCreation l left join fetch l.currentLimits1 left join fetch l.currentLimits2 WHERE line.id = ?1")
    ModificationEntity findLineAttachToVoltageLevelEntityCreationById(UUID uuid);

}
