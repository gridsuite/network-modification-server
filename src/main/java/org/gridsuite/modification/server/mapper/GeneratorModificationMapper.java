package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.GeneratorModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorModificationEntity;

public class GeneratorModificationMapper extends ModificationMapper<GeneratorModificationInfos, GeneratorModificationEntity> {
    public GeneratorModificationMapper() {
        super(GeneratorModificationEntity.class, GeneratorModificationInfos.class);
    }
}
