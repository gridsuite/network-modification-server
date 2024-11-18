/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentLimitsEntity;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentTemporaryLimitCreationEmbeddable;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class CurrentLimitsMapper implements EntityMapper<CurrentLimitsInfos, CurrentLimitsEntity> {

    @Override
    public CurrentLimitsEntity toEntity(CurrentLimitsInfos currentLimitsInfos) {
        if (currentLimitsInfos == null) {
            return null;
        }
        return new CurrentLimitsEntity(null, currentLimitsInfos.getPermanentLimit(), CurrentTemporaryLimitCreationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsInfos.getTemporaryLimits()));
    }

    @Override
    public CurrentLimitsInfos toDto(CurrentLimitsEntity entity) {
        // TODO this method will replace the toModificationInfos method in entity classes
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
