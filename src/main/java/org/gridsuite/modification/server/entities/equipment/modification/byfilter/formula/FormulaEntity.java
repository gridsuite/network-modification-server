/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.entities.equipment.modification.byfilter.formula;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Index;
import jakarta.persistence.Table;
import lombok.NoArgsConstructor;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;
import org.gridsuite.modification.server.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.entities.equipment.modification.byfilter.ModificationByFilterEntity;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@NoArgsConstructor
@Entity
@Table(name = "formula", indexes = @Index(name = "modification_by_filter_id_idx", columnList = "modification_by_filter_id"))
public class FormulaEntity extends ModificationByFilterEntity {
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
    }

    public FormulaInfos toFormulaInfos() {
        FormulaInfos formulaInfos = FormulaInfos.builder()
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
