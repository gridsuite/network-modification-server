package org.gridsuite.modification.server.dto;

import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonTypeName;
import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.annotation.ModificationErrorTypeName;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.entities.equipment.modification.ByFormulaModificationEntity;
import org.gridsuite.modification.server.modifications.ByFormulaModification;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@JsonTypeName("BY_FORMULA_MODIFICATION")
@ModificationErrorTypeName("BY_FORMULA_MODIFICATION_ERROR")
public class ByFormulaModificationInfos extends ModificationInfos {
    @Schema(description = "Identifiable type")
    private IdentifiableType identifiableType;

    @Schema(description = "list of formulas")
    private List<FormulaInfos> formulaInfosList;

    @Override
    public ByFormulaModificationEntity toEntity() {
        return new ByFormulaModificationEntity(this);
    }

    @Override
    public ByFormulaModification toModification() {
        return new ByFormulaModification(this);
    }
}
