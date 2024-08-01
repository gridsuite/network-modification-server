/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.byfilter.formula;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.byfilter.FilterModificationInfos;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula.FormulaEntity;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@Data
public class FormulaInfos extends FilterModificationInfos {

    @Schema(description = "First reference field or value")
    private ReferenceFieldOrValue fieldOrValue1;

    @Schema(description = "Second reference field or value")
    private ReferenceFieldOrValue fieldOrValue2;

    @Schema(description = "Operator")
    private Operator operator;

    public FormulaEntity toEntity() {
        return new FormulaEntity(this);
    }
}
