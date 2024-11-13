/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class FreePropertyMapper implements EntityMapper<FreePropertyInfos, FreePropertyEntity> {
    @Override
    public FreePropertyEntity toEntity(FreePropertyInfos freePropertyInfos) {
        return FreePropertyEntity.builder()
            .name(freePropertyInfos.getName())
            .value(freePropertyInfos.getValue())
            .deletionMark(freePropertyInfos.isDeletionMark())
            .added(freePropertyInfos.isAdded())
            .previousValue(freePropertyInfos.getPreviousValue())
            .build();
    }

    @Override
    public FreePropertyInfos toDto(FreePropertyEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
