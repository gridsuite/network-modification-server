package org.gridsuite.modification.server.dto.formula;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.equipmentfield.EquipmentField;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class FormulaInfos {
    @Schema(description = "List of filters")
    private List<FilterInfos> filters;

    @Schema(description = "Equipment field")
    private EquipmentField equipmentField;

    @Schema(description = "First reference field or value")
    private ReferenceFieldOrValue fieldOrValue1;

    @Schema(description = "Second reference field or value")
    private ReferenceFieldOrValue fieldOrValue2;

    @Schema(description = "Operator")
    private Operator operator;
}
