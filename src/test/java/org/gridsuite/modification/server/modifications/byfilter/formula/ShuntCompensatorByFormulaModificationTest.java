/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.byfilter.equipmentfield.ShuntCompensatorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.modifications.data.assignment.Operator;
import org.gridsuite.modification.modifications.data.assignment.ReferenceFieldOrValue;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.utils.NetworkUtil.createShuntCompensator;
import static org.junit.jupiter.api.Assertions.assertEquals;

class ShuntCompensatorByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String SHUNT_COMPENSATOR_ID_1 = "v1shunt";
    private static final String SHUNT_COMPENSATOR_ID_2 = "v2shunt";
    private static final String SHUNT_COMPENSATOR_ID_3 = "v3shunt";
    private static final String SHUNT_COMPENSATOR_ID_4 = "v4shunt";
    private static final String SHUNT_COMPENSATOR_ID_5 = "v5shunt";
    private static final String SHUNT_COMPENSATOR_ID_6 = "v6shunt";

    @Override
    protected void createEquipments() {
        createShuntCompensator(getNetwork().getVoltageLevel("v1"), SHUNT_COMPENSATOR_ID_1, "v1shunt", 8, 225., 10, true, 4, 2, 3, 2, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v3"), SHUNT_COMPENSATOR_ID_3, "v3shunt", 10, 305., 20, true, 6, 3, 3, 4, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
        createShuntCompensator(getNetwork().getVoltageLevel("v4"), SHUNT_COMPENSATOR_ID_4, "v3shunt", 10, 305., 20, true, 15, 4, 3, 10, "cn11", 22, ConnectablePosition.Direction.BOTTOM);
    }

    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(SHUNT_COMPENSATOR_ID_1, SHUNT_COMPENSATOR_ID_2),
            FILTER_ID_2, Set.of(SHUNT_COMPENSATOR_ID_3, SHUNT_COMPENSATOR_ID_6),
            FILTER_ID_3, Set.of(SHUNT_COMPENSATOR_ID_4, SHUNT_COMPENSATOR_ID_5),
            FILTER_ID_4, Set.of(SHUNT_COMPENSATOR_ID_1, SHUNT_COMPENSATOR_ID_5),
            FILTER_ID_5, Set.of(SHUNT_COMPENSATOR_ID_3, SHUNT_COMPENSATOR_ID_2)
    );

    @Override
    protected Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
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
    protected void assertAfterNetworkModificationCreation() {
        ShuntCompensator shuntCompensator1 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1);
        assertEquals(8, shuntCompensator1.getMaximumSectionCount());
        assertEquals(1.625, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator2 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_2);
        assertEquals(6, shuntCompensator2.getMaximumSectionCount());
        assertEquals(0.25, shuntCompensator2.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator3 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_3);
        assertEquals(12, shuntCompensator3.getMaximumSectionCount());
        assertEquals(0.75, shuntCompensator3.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator4 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_4);
        assertEquals(13, shuntCompensator4.getSectionCount());

        ShuntCompensator shuntCompensator5 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_5);
        assertEquals(1, shuntCompensator5.getSectionCount());

        ShuntCompensator shuntCompensator6 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_6);
        assertEquals(6, shuntCompensator6.getMaximumSectionCount());
        assertEquals(0.5, shuntCompensator6.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        ShuntCompensator shuntCompensator1 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1);
        assertEquals(4, shuntCompensator1.getMaximumSectionCount());
        assertEquals(2, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator2 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_2);
        assertEquals(3, shuntCompensator2.getMaximumSectionCount());
        assertEquals(1, shuntCompensator2.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator3 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_3);
        assertEquals(6, shuntCompensator3.getMaximumSectionCount());
        assertEquals(3, shuntCompensator3.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator4 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_4);
        assertEquals(10, shuntCompensator4.getSectionCount());

        ShuntCompensator shuntCompensator5 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_5);
        assertEquals(2, shuntCompensator5.getSectionCount());
        assertEquals(1, shuntCompensator5.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator6 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_6);
        assertEquals(3, shuntCompensator6.getMaximumSectionCount());
        assertEquals(1, shuntCompensator6.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);
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
