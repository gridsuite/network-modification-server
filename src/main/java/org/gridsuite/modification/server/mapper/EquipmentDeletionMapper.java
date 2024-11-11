package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.EquipmentDeletionInfos;
import org.gridsuite.modification.server.entities.equipment.deletion.EquipmentDeletionEntity;

public class EquipmentDeletionMapper extends ModificationMapper<EquipmentDeletionInfos, EquipmentDeletionEntity> {
    public EquipmentDeletionMapper() {
        super(EquipmentDeletionEntity.class, EquipmentDeletionInfos.class);
    }
}
