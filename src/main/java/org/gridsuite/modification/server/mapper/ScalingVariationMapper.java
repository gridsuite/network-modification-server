package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.ScalingVariationEntity;

public class ScalingVariationMapper extends ModificationMapper<ScalingVariationInfos, ScalingVariationEntity> {
    public ScalingVariationMapper() {
        super(ScalingVariationEntity.class, ScalingVariationInfos.class);
    }
}
