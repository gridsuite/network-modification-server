package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.TwoWindingsTransformerModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.TwoWindingsTransformerModificationEntity;

public class TwoWindingsTransformerModificationMapper extends ModificationMapper<TwoWindingsTransformerModificationInfos, TwoWindingsTransformerModificationEntity> {
    public TwoWindingsTransformerModificationMapper() {
        super(TwoWindingsTransformerModificationEntity.class, TwoWindingsTransformerModificationInfos.class);
    }
}
