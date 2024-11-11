package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ShuntCompensatorModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.ShuntCompensatorModificationEntity;

public class ShuntCompensatorModificationMapper extends ModificationMapper<ShuntCompensatorModificationInfos, ShuntCompensatorModificationEntity> {
    public ShuntCompensatorModificationMapper() {
        super(ShuntCompensatorModificationEntity.class, ShuntCompensatorModificationInfos.class);
    }
}
