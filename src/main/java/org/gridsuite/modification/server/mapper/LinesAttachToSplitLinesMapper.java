package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LinesAttachToSplitLinesInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LinesAttachToSplitLinesEntity;

public class LinesAttachToSplitLinesMapper extends ModificationMapper<LinesAttachToSplitLinesInfos, LinesAttachToSplitLinesEntity> {
    public LinesAttachToSplitLinesMapper() {
        super(LinesAttachToSplitLinesEntity.class, LinesAttachToSplitLinesInfos.class);
    }
}
