package org.gridsuite.modification.server.entities.equipment.modification.byfilter;

import com.powsybl.iidm.network.IdentifiableType;
import jakarta.persistence.*;
import lombok.*;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula.FormulaEntity;

import java.util.List;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "byFormulaModification")
public class ByFormulaModificationEntity extends ModificationEntity {
    @Column
    private IdentifiableType identifiableType;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "by_formula_modification_id",
            foreignKey = @ForeignKey(name = "by_formula_modification_id_fk"))
    private List<FormulaEntity> formulaEntities;

    public ByFormulaModificationEntity(ByFormulaModificationInfos byFormulaModificationInfos) {
        super(byFormulaModificationInfos);
        assignAttributes(byFormulaModificationInfos);
    }

    @Override
    public void update(@NonNull ModificationInfos modificationInfos) {
        super.update(modificationInfos);
        assignAttributes((ByFormulaModificationInfos) modificationInfos);
    }

    private void assignAttributes(ByFormulaModificationInfos byFormulaModificationInfos) {
        this.identifiableType = byFormulaModificationInfos.getIdentifiableType();
        if (formulaEntities == null) {
            formulaEntities = byFormulaModificationInfos.getFormulaInfosList()
                    .stream()
                    .map(FormulaEntity::new)
                    .toList();
        } else {
            formulaEntities.clear();
            formulaEntities.addAll(byFormulaModificationInfos.getFormulaInfosList()
                    .stream()
                    .map(FormulaEntity::new)
                    .toList());
        }
    }

    @Override
    public ByFormulaModificationInfos toModificationInfos() {
        return toByFormulaModificationInfosBuilder().build();
    }

    private ByFormulaModificationInfos.ByFormulaModificationInfosBuilder<?, ?> toByFormulaModificationInfosBuilder() {
        return ByFormulaModificationInfos.builder()
                .uuid(getId())
                .date(getDate())
                .stashed(getStashed())
                .activated(getActivated())
                .identifiableType(getIdentifiableType())
                .formulaInfosList(getFormulaEntities().stream()
                        .map(FormulaEntity::toFormulaInfos)
                        .collect(Collectors.toList()));
    }
}
