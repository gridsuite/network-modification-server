package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteAttachingLineEntity;

public class DeleteAttachingLineMapper extends ModificationMapper<DeleteAttachingLineInfos, DeleteAttachingLineEntity> {
    public DeleteAttachingLineMapper() {
        super(DeleteAttachingLineEntity.class, DeleteAttachingLineInfos.class);
    }
}
