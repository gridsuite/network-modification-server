package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LineAttachToVoltageLevelInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LineAttachToVoltageLevelEntity;

public class LineAttachToVoltageLevelMapper extends ModificationMapper<LineAttachToVoltageLevelInfos, LineAttachToVoltageLevelEntity> {
    public LineAttachToVoltageLevelMapper() {
        super(LineAttachToVoltageLevelEntity.class, LineAttachToVoltageLevelInfos.class);
    }
}
