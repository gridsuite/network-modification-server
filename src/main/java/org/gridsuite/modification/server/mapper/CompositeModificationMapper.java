package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.server.entities.CompositeModificationEntity;

public class CompositeModificationMapper extends ModificationMapper<CompositeModificationInfos, CompositeModificationEntity> {
    public CompositeModificationMapper() {
        super(CompositeModificationEntity.class, CompositeModificationInfos.class);
    }
}
