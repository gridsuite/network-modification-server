package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula.FormulaEntity;

public class FormulaMapper extends ModificationMapper<FormulaInfos, FormulaEntity> {
    public FormulaMapper() {
        super(FormulaEntity.class, FormulaInfos.class);
    }
}
