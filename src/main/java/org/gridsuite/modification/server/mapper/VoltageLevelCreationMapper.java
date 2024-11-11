package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.VoltageLevelCreationEntity;

public class VoltageLevelCreationMapper extends ModificationMapper<VoltageLevelCreationInfos, VoltageLevelCreationEntity> {
    public VoltageLevelCreationMapper() {
        super(VoltageLevelCreationEntity.class, VoltageLevelCreationInfos.class);
    }
}
