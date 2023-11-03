/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Getter
@Setter
@Entity
@Table(name = "formula", indexes = @Index(name = "by_formula_modification_id_idx", columnList = "by_formula_modification_id"))
public class FormulaEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id")
    private UUID id;

    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "formula_id",
            foreignKey = @ForeignKey(name = "formula_id_fk"))
    private List<VariationFilterEntity> filters;

    @Column
    private String editedField;

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
        this.id = null;
        if (filters == null) {
            this.filters = formulaInfos.getFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList());
        } else {
            filters.clear();
            filters.addAll(formulaInfos.getFilters().stream().map(FilterInfos::toEntity).collect(Collectors.toList()));
        }
        this.editedField = formulaInfos.getEditedField();
        this.equipmentField1 = formulaInfos.getFieldOrValue1().getEquipmentField();
        this.equipmentField2 = formulaInfos.getFieldOrValue2().getEquipmentField();
        this.value1 = formulaInfos.getFieldOrValue1().getValue();
        this.value2 = formulaInfos.getFieldOrValue2().getValue();
        this.operator = formulaInfos.getOperator();
    }

    public FormulaInfos toFormulaInfos() {
        return FormulaInfos.builder()
                .id(getId())
                .filters(getFilters().stream()
                        .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                        .collect(Collectors.toList()))
                .editedField(getEditedField())
                .fieldOrValue1(ReferenceFieldOrValue.builder()
                        .equipmentField(getEquipmentField1())
                        .value(getValue1())
                        .build())
                .fieldOrValue2(ReferenceFieldOrValue.builder()
                        .equipmentField(getEquipmentField2())
                        .value(getValue2())
                        .build())
                .operator(getOperator())
                .build();
    }
}
