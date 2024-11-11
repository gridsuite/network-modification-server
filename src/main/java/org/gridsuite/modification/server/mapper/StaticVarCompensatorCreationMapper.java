package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.StaticVarCompensatorCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.StaticCompensatorCreationEntity;

public class StaticVarCompensatorCreationMapper extends ModificationMapper<StaticVarCompensatorCreationInfos, StaticCompensatorCreationEntity> {
    public StaticVarCompensatorCreationMapper() {
        super(StaticCompensatorCreationEntity.class, StaticVarCompensatorCreationInfos.class);
    }
}
