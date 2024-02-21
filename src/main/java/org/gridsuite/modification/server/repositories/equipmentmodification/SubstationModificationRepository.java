/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories.equipmentmodification;

import org.gridsuite.modification.server.entities.equipment.modification.SubstationModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Repository
public interface SubstationModificationRepository extends JpaRepository<SubstationModificationEntity, UUID>, EquipmentModificationRepository {
    @Override
    @Modifying
    @Query(value = "BEGIN;" +
            "DELETE FROM substation_modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "COMMIT;", nativeQuery = true)
    void deleteSubModificationsByIds(List<UUID> subModificationIds, UUID tabularModificationId);
}
