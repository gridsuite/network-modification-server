/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.assignment.AssignmentEntity;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class PropertyAssignmentMapper implements EntityMapper<PropertyAssignmentInfos, AssignmentEntity> {
    @Override
    public AssignmentEntity toEntity(PropertyAssignmentInfos propertyAssignmentInfos) {
        AssignmentEntity entity = new AssignmentEntity(propertyAssignmentInfos);
        entity.setPropertyName(propertyAssignmentInfos.getPropertyName());
        return entity;
    }

    @Override
    public PropertyAssignmentInfos toDto(AssignmentEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
