package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.OperatingStatusModificationEntity;

public class OperatingStatusModificationMapper extends ModificationMapper<OperatingStatusModificationInfos, OperatingStatusModificationEntity> {
    public OperatingStatusModificationMapper() {
        super(OperatingStatusModificationEntity.class, OperatingStatusModificationInfos.class);
    }
}
