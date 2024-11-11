package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ShuntCompensatorCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ShuntCompensatorCreationEntity;

public class ShuntCompensatorCreationMapper extends ModificationMapper<ShuntCompensatorCreationInfos, ShuntCompensatorCreationEntity> {
    public ShuntCompensatorCreationMapper() {
        super(ShuntCompensatorCreationEntity.class, ShuntCompensatorCreationInfos.class);
    }
}
