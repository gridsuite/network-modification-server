package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LineCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.LineCreationEntity;

public class LineCreationMapper extends ModificationMapper<LineCreationInfos, LineCreationEntity> {
    public LineCreationMapper() {
        super(LineCreationEntity.class, LineCreationInfos.class);
    }
}
