/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentLimitsModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentTemporaryLimitModificationEmbeddable;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class CurrentLimitsModificationMapper implements EntityMapper<CurrentLimitsModificationInfos, CurrentLimitsModificationEntity> {
    @Override
    public CurrentLimitsModificationEntity toEntity(CurrentLimitsModificationInfos currentLimitsModificationInfos) {
        if (currentLimitsModificationInfos == null) {
            return null;
        }
        return new CurrentLimitsModificationEntity(null, currentLimitsModificationInfos.getPermanentLimit(),
            CurrentTemporaryLimitModificationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsModificationInfos.getTemporaryLimits()));
    }

    @Override
    public CurrentLimitsModificationInfos toDto(CurrentLimitsModificationEntity entity) {
        // TODO this method will replace the toModificationInfos method in entity classes
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
