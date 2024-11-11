package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.assignment.AssignmentEntity;

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
