package org.gridsuite.modification.server.dto;

import com.powsybl.iidm.network.IdentifiableType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.entities.equipment.modification.ByFormulaModificationEntity;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class ByFormulaModificationInfos extends ModificationInfos {
    @Schema(description = "Identifiable type")
    private IdentifiableType identifiableType;

    @Schema(description = "list of formulas")
    private List<FormulaInfos> formulaInfosList;

    @Override
    public ByFormulaModificationEntity toEntity() {
        return new ByFormulaModificationEntity(this);
    }
}
