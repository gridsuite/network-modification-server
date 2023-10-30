/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.dto.formula;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.entities.equipment.modification.FormulaEntity;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class FormulaInfos {
    @Schema(description = "List of filters")
    private List<FilterInfos> filters;

    @Schema(description = "Edited field")
    private String editedField;

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
