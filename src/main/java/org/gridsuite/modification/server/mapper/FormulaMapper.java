/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.mapper;

import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula.FormulaEntity;

/**
 * @author Ayoub LABIDI <ayoub.labidi at rte-france.com>
 */
public class FormulaMapper extends ModificationMapper<FormulaInfos, FormulaEntity> {
    public FormulaMapper() {
        super(FormulaEntity.class, FormulaInfos.class);
    }
}
