package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ConverterStationCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.ConverterStationCreationEntity;

public class ConverterStationCreationMapper extends ModificationMapper<ConverterStationCreationInfos, ConverterStationCreationEntity> {
    public ConverterStationCreationMapper() {
        super(ConverterStationCreationEntity.class, ConverterStationCreationInfos.class);
    }
}
