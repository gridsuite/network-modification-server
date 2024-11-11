package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.CurrentLimitsInfos;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentLimitsEntity;
import org.gridsuite.modification.server.entities.equipment.creation.CurrentTemporaryLimitCreationEmbeddable;

public class CurrentLimitsMapper implements EntityMapper<CurrentLimitsInfos, CurrentLimitsEntity> {

    @Override
    public CurrentLimitsEntity toEntity(CurrentLimitsInfos currentLimitsInfos) {
        if (currentLimitsInfos == null) {
            return null;
        }
        return new CurrentLimitsEntity(null, currentLimitsInfos.getPermanentLimit(), CurrentTemporaryLimitCreationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsInfos.getTemporaryLimits()));
    }

    @Override
    public CurrentLimitsInfos toDto(CurrentLimitsEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
