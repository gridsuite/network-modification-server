package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.VoltageInitModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VoltageInitModificationEntity;

public class VoltageInitModificationMapper extends ModificationMapper<VoltageInitModificationInfos, VoltageInitModificationEntity> {
    public VoltageInitModificationMapper() {
        super(VoltageInitModificationEntity.class, VoltageInitModificationInfos.class);
    }
}
