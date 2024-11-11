package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.entities.equipment.modification.GeneratorScalingEntity;

public class GeneratorScalingMapper extends ModificationMapper<GeneratorScalingInfos, GeneratorScalingEntity> {
    public GeneratorScalingMapper() {
        super(GeneratorScalingEntity.class, GeneratorScalingInfos.class);
    }
}
