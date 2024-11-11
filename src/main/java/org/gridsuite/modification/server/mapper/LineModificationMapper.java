package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LineModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LineModificationEntity;

public class LineModificationMapper extends ModificationMapper<LineModificationInfos, LineModificationEntity> {
    public LineModificationMapper() {
        super(LineModificationEntity.class, LineModificationInfos.class);
    }
}
