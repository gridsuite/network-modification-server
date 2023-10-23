package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.equipmentfield.EquipmentField;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@NoArgsConstructor
@Getter
@Entity
@Table(name = "formula")
public class FormulaEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinTable(
            joinColumns = @JoinColumn(name = "id"),
            inverseJoinColumns = @JoinColumn(name = "filterId"))
    private List<VariationFilterEntity> filters;

    @Column
    private String equipmentField1;

    @Column
    private Double value1;

    @Column
    private String equipmentField2;

    @Column
    private Double value2;

    @Column
    private Operator operator;

    public FormulaEntity(FormulaInfos formulaInfos) {
        this.filters = formulaInfos.getFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList());
        this.equipmentField1 = formulaInfos.getFieldOrValue1().getEquipmentField().toString();
        this.equipmentField2 = formulaInfos.getFieldOrValue2().getEquipmentField().toString();
        this.value1 = formulaInfos.getFieldOrValue1().getValue();
        this.value2 = formulaInfos.getFieldOrValue2().getValue();
        this.operator = formulaInfos.getOperator();
    }
}
