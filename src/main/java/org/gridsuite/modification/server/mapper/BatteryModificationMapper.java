package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.BatteryModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.BatteryModificationEntity;

public class BatteryModificationMapper extends ModificationMapper<BatteryModificationInfos, BatteryModificationEntity> {
    public BatteryModificationMapper() {
        super(BatteryModificationEntity.class, BatteryModificationInfos.class);
    }
}
