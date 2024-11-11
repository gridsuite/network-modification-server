package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.entities.equipment.modification.DeleteVoltageLevelOnLineEntity;

public class DeleteVoltageLevelOnLineMapper extends ModificationMapper<DeleteVoltageLevelOnLineInfos, DeleteVoltageLevelOnLineEntity> {
    public DeleteVoltageLevelOnLineMapper() {
        super(DeleteVoltageLevelOnLineEntity.class, DeleteVoltageLevelOnLineInfos.class);
    }
}
