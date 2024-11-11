package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.VoltageLevelModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VoltageLevelModificationEntity;

public class VoltageLevelModificationMapper extends ModificationMapper<VoltageLevelModificationInfos, VoltageLevelModificationEntity> {
    public VoltageLevelModificationMapper() {
        super(VoltageLevelModificationEntity.class, VoltageLevelModificationInfos.class);
    }
}
