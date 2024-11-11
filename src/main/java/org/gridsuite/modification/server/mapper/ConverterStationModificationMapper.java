package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ConverterStationModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.ConverterStationModificationEntity;

public class ConverterStationModificationMapper extends ModificationMapper<ConverterStationModificationInfos, ConverterStationModificationEntity> {
    public ConverterStationModificationMapper() {
        super(ConverterStationModificationEntity.class, ConverterStationModificationInfos.class);
    }
}
