/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.repositories.equipmentmodification;

import org.gridsuite.modification.server.entities.equipment.modification.TwoWindingsTransformerModificationEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.UUID;

/**
 * @author Etienne Homer <etienne.homer at rte-france.com>
 */
@Repository
public interface TwoWindingsTransformerModificationRepository extends JpaRepository<TwoWindingsTransformerModificationEntity, UUID> {

}
