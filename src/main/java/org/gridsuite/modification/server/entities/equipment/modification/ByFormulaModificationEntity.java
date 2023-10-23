package org.gridsuite.modification.server.entities.equipment.modification;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;

import java.util.List;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "byFormulaModification")
public class ByFormulaModificationEntity extends ModificationEntity {
    @Column
    private IdentifiableType identifiableType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private List<FormulaEntity> formulaEntities;

    public ByFormulaModificationEntity(ByFormulaModificationInfos byFormulaModificationInfos) {
        super(byFormulaModificationInfos);
        assignAttributes(byFormulaModificationInfos);
    }

    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ByFormulaModificationInfos) modificationInfos);
    }

    private void assignAttributes(ByFormulaModificationInfos byFormulaModificationInfos) {
        this.identifiableType = byFormulaModificationInfos.getIdentifiableType();
        this.formulaEntities = byFormulaModificationInfos.getFormulaInfosList().stream()
                .map(FormulaEntity::new)
                .collect(Collectors.toList());
    }
}
