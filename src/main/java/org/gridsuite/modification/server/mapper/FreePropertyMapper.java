package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FreePropertyEntity;

public class FreePropertyMapper implements EntityMapper<FreePropertyInfos, FreePropertyEntity> {
    @Override
    public FreePropertyEntity toEntity(FreePropertyInfos freePropertyInfos) {
        return FreePropertyEntity.builder()
            .name(freePropertyInfos.getName())
            .value(freePropertyInfos.getValue())
            .deletionMark(freePropertyInfos.isDeletionMark())
            .added(freePropertyInfos.isAdded())
            .previousValue(freePropertyInfos.getPreviousValue())
            .build();
    }

    @Override
    public FreePropertyInfos toDto(FreePropertyEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
