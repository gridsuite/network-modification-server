package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.BatteryCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.BatteryCreationEntity;

public class BatteryCreationMapper extends ModificationMapper<BatteryCreationInfos, BatteryCreationEntity> {
    public BatteryCreationMapper() {
        super(BatteryCreationEntity.class, BatteryCreationInfos.class);
    }
}
