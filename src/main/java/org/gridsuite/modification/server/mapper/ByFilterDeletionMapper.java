package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.server.entities.equipment.deletion.ByFilterDeletionEntity;

public class ByFilterDeletionMapper extends ModificationMapper<ByFilterDeletionInfos, ByFilterDeletionEntity> {
    public ByFilterDeletionMapper() {
        super(ByFilterDeletionEntity.class, ByFilterDeletionInfos.class);
    }
}
