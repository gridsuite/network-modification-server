package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LoadModificationEntity;

public class LoadModificationMapper extends ModificationMapper<LoadModificationInfos, LoadModificationEntity> {
    public LoadModificationMapper() {
        super(LoadModificationEntity.class, LoadModificationInfos.class);
    }
}
