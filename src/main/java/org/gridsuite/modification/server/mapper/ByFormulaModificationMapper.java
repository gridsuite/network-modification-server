package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ByFormulaModificationEntity;

public class ByFormulaModificationMapper extends ModificationMapper<ByFormulaModificationInfos, ByFormulaModificationEntity> {
    public ByFormulaModificationMapper() {
        super(ByFormulaModificationEntity.class, ByFormulaModificationInfos.class);
    }
}
