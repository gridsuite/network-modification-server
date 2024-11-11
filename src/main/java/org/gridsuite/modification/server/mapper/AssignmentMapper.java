package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.assignment.AssignmentEntity;

public class AssignmentMapper implements EntityMapper<AssignmentInfos<?>, AssignmentEntity> {
    @Override
    public AssignmentEntity toEntity(AssignmentInfos<?> assignmentInfos) {
        return new AssignmentEntity(assignmentInfos);
    }

    @Override
    public AssignmentInfos<?> toDto(AssignmentEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
