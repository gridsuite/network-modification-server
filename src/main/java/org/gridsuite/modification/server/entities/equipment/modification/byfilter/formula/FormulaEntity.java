/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula;

import jakarta.persistence.*;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;
import org.gridsuite.modification.server.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.entities.equipment.modification.VariationFilterEntity;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.AbstractAssignmentEntity;

import java.util.List;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Entity
@Table(name = "formula", indexes = @Index(name = "by_formula_modification_id_idx", columnList = "by_formula_modification_id"))
public class FormulaEntity extends AbstractAssignmentEntity {
    @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    @JoinColumn(name = "formula_id",
            foreignKey = @ForeignKey(name = "formula_id_fk"))
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
        super(formulaInfos);
        this.equipmentField1 = formulaInfos.getFieldOrValue1().getEquipmentField();
        this.equipmentField2 = formulaInfos.getFieldOrValue2().getEquipmentField();
        this.value1 = formulaInfos.getFieldOrValue1().getValue();
        this.value2 = formulaInfos.getFieldOrValue2().getValue();
        this.operator = formulaInfos.getOperator();
        this.filters = formulaInfos.getFilters().stream().map(FilterInfos::toEntity).toList();
    }

    public FormulaInfos toFormulaInfos() {
        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(filters.stream()
                        .map(filterEntity -> new FilterInfos(filterEntity.getFilterId(), filterEntity.getName()))
                        .toList())
                .fieldOrValue1(ReferenceFieldOrValue.builder()
                        .equipmentField(equipmentField1)
                        .value(value1)
                        .build())
                .fieldOrValue2(ReferenceFieldOrValue.builder()
                        .equipmentField(equipmentField2)
                        .value(value2)
                        .build())
                .operator(operator)
                .build();
        assignAttributes(formulaInfos);
        return formulaInfos;
    }
}
