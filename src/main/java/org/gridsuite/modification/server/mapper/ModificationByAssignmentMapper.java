package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ModificationByAssignmentEntity;

public class ModificationByAssignmentMapper extends ModificationMapper<ModificationByAssignmentInfos, ModificationByAssignmentEntity> {
    public ModificationByAssignmentMapper() {
        super(ModificationByAssignmentEntity.class, ModificationByAssignmentInfos.class);
    }
}
