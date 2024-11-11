package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.VscCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.VscCreationEntity;

public class VscCreationMapper extends ModificationMapper<VscCreationInfos, VscCreationEntity> {
    public VscCreationMapper() {
        super(VscCreationEntity.class, VscCreationInfos.class);
    }
}
