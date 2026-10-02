/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createShuntCompensator;

class ShuntCompensatorByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String SHUNT_COMPENSATOR_ID_1 = "v1shunt";
    private static final String SHUNT_COMPENSATOR_ID_2 = "v2shunt";
    private static final String SHUNT_COMPENSATOR_ID_3 = "v3shunt";
    private static final String SHUNT_COMPENSATOR_ID_4 = "v4shunt";
    private static final String SHUNT_COMPENSATOR_ID_5 = "v5shunt";
    private static final String SHUNT_COMPENSATOR_ID_6 = "v6shunt";

    @Test
    void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(SHUNT_COMPENSATOR_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(2.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(3.).build())
                .build();

        checkCreateWithWarning(List.of(formulaInfos), List.of(identifiableAttributes, wrongIdAttributes));
    }

    @Override
    protected void createEquipments() {
        createShuntCompensator(getNetwork().getVoltageLevel("v1"), SHUNT_COMPENSATOR_ID_1, "v1shunt", 8, 225., 10, true, 4, 2, 3, 2, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v3"), SHUNT_COMPENSATOR_ID_3, "v3shunt", 10, 305., 20, true, 6, 3, 3, 4, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v4"), SHUNT_COMPENSATOR_ID_4, "v3shunt", 10, 305., 20, true, 15, 4, 3, 10, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_5, 6.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_5, 6.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_2, 2.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name(),
                List.of(filter1, filter2),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name()).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(ShuntCompensatorField.SECTION_COUNT.name(),
                List.of(filter3),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos3 = getFormulaInfo(ShuntCompensatorField.MAX_SUSCEPTANCE.name(),
                List.of(filter4),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAX_SUSCEPTANCE.name()).build(),
                ReferenceFieldOrValue.builder().value(5.).build());

        FormulaInfos formulaInfos4 = getFormulaInfo(ShuntCompensatorField.MAX_Q_AT_NOMINAL_V.name(),
                List.of(filter5),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAX_Q_AT_NOMINAL_V.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4);
    }

    @Override
    protected List<FormulaInfos> getUpdatedFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name(),
                List.of(filter1, filter2),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(150.).build(),
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name()).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(ShuntCompensatorField.SECTION_COUNT.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(ShuntCompensatorField.SECTION_COUNT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());
        return List.of(formulaInfos1, formulaInfos2);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.SHUNT_COMPENSATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.SHUNT_COMPENSATOR;
    }
}
