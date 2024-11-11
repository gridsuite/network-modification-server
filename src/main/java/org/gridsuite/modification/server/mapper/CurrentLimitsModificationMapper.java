package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.CurrentLimitsModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentLimitsModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.CurrentTemporaryLimitModificationEmbeddable;

public class CurrentLimitsModificationMapper implements EntityMapper<CurrentLimitsModificationInfos, CurrentLimitsModificationEntity> {
    @Override
    public CurrentLimitsModificationEntity toEntity(CurrentLimitsModificationInfos currentLimitsModificationInfos) {
        if (currentLimitsModificationInfos == null) {
            return null;
        }
        return new CurrentLimitsModificationEntity(null, currentLimitsModificationInfos.getPermanentLimit(),
            CurrentTemporaryLimitModificationEmbeddable.toEmbeddableCurrentTemporaryLimits(currentLimitsModificationInfos.getTemporaryLimits()));
    }

    @Override
    public CurrentLimitsModificationInfos toDto(CurrentLimitsModificationEntity entity) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'toDto'");
    }
}
