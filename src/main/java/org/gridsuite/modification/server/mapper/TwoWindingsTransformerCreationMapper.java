package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.TwoWindingsTransformerCreationInfos;
import org.gridsuite.modification.server.entities.equipment.creation.TwoWindingsTransformerCreationEntity;

public class TwoWindingsTransformerCreationMapper extends ModificationMapper<TwoWindingsTransformerCreationInfos, TwoWindingsTransformerCreationEntity> {
    public TwoWindingsTransformerCreationMapper() {
        super(TwoWindingsTransformerCreationEntity.class, TwoWindingsTransformerCreationInfos.class);
    }
}
