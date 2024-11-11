package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.SubstationModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.SubstationModificationEntity;

public class SubstationModificationMapper extends ModificationMapper<SubstationModificationInfos, SubstationModificationEntity> {
    public SubstationModificationMapper() {
        super(SubstationModificationEntity.class, SubstationModificationInfos.class);
    }
}
