package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.LoadCreationEntity;

public class LoadCreationMapper extends ModificationMapper<LoadCreationInfos, LoadCreationEntity> {
    public LoadCreationMapper() {
        super(LoadCreationEntity.class, LoadCreationInfos.class);
    }
}
