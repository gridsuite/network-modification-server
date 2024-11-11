package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.GeneratorCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.GeneratorCreationEntity;

public class GeneratorCreationMapper extends ModificationMapper<GeneratorCreationInfos, GeneratorCreationEntity> {
    public GeneratorCreationMapper() {
        super(GeneratorCreationEntity.class, GeneratorCreationInfos.class);
    }
}
