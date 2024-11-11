package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LineSplitWithVoltageLevelEntity;

public class LineSplitWithVoltageLevelMapper extends ModificationMapper<LineSplitWithVoltageLevelInfos, LineSplitWithVoltageLevelEntity> {
    public LineSplitWithVoltageLevelMapper() {
        super(LineSplitWithVoltageLevelEntity.class, LineSplitWithVoltageLevelInfos.class);
    }
}
