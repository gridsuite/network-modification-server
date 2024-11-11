package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;

public class VariationFilterMapper extends ModificationMapper<FilterInfos, VariationFilterEntity> {
    public VariationFilterMapper() {
        super(VariationFilterEntity.class, FilterInfos.class);
    }
}
