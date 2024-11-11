package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.VscModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VscModificationEntity;

public class VscModificationMapper extends ModificationMapper<VscModificationInfos, VscModificationEntity> {
    public VscModificationMapper() {
        super(VscModificationEntity.class, VscModificationInfos.class);
    }
}
