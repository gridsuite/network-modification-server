package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.LoadScalingInfos;
import org.gridsuite.modification.server.entities.equipment.modification.LoadScalingEntity;

public class LoadScalingMapper extends ModificationMapper<LoadScalingInfos, LoadScalingEntity> {
    public LoadScalingMapper() {
        super(LoadScalingEntity.class, LoadScalingInfos.class);
    }
}
