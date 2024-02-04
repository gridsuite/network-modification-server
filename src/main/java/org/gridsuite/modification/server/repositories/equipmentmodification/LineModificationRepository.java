/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories.equipmentmodification;

import org.gridsuite.modification.server.entities.equipment.modification.LineModificationEntity;
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
public interface LineModificationRepository extends JpaRepository<LineModificationEntity, UUID>, EquipmentModificationRepository {
    // To disable trigger is a bad practice and in production, the postgres user of the application will not be allowed to change the schema.
    // This code is just an example of how fixing the following problem :
    // When we want to delete a tabular modification, we need to execute a delete statement with thousands of ids on the 'modification' table.
    // But as the 'modification' table is referenced up to now by 40 foreign key constraints (it will be about 300 later), all the triggers make the delete statement execution quite slow (about 3-4 seconds).
    @Override
    @Modifying
    @Query(value = "BEGIN;" +
            "ALTER TABLE modification DISABLE TRIGGER ALL;" +
            "DELETE FROM line_modification WHERE id IN ?1 ;" +
            "DELETE FROM tabular_modification_modifications WHERE tabular_modification_entity_id = ?2 ;" +
            "DELETE FROM modification WHERE id IN ?1 ;" +
            "ALTER TABLE modification ENABLE TRIGGER ALL;" +
            "COMMIT;", nativeQuery = true)
    void deleteSubModificationsByIds(List<UUID> subModificationIds, UUID tabularModificationId);
}
