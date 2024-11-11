package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.SubstationCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.SubstationCreationEntity;

public class SubstationCreationMapper extends ModificationMapper<SubstationCreationInfos, SubstationCreationEntity> {
    public SubstationCreationMapper() {
        super(SubstationCreationEntity.class, SubstationCreationInfos.class);
    }
}
