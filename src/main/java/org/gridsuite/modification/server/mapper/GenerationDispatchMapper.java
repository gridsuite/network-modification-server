package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.entities.equipment.modification.GenerationDispatchEntity;

public class GenerationDispatchMapper extends ModificationMapper<GenerationDispatchInfos, GenerationDispatchEntity> {
    public GenerationDispatchMapper() {
        super(GenerationDispatchEntity.class, GenerationDispatchInfos.class);
    }
}
