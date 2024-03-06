/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.formula.equipmentfield.LoadField;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;

import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.junit.Assert.assertEquals;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class LoadByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";

    @Override
    protected void createEquipments() {
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_1, "load1", 100, 100, 120, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_2, "load2", 200, 80, 90, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_3, "load3", 300, 100, 70, null, 5, null);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_4, "load4", 400, 50, 150, null, 5, null);
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes load1 = getIdentifiableAttributes(LOAD_ID_1, 1.0);
        IdentifiableAttributes load2 = getIdentifiableAttributes(LOAD_ID_2, 2.0);
        IdentifiableAttributes load3 = getIdentifiableAttributes(LOAD_ID_3, 2.0);
        IdentifiableAttributes load4 = getIdentifiableAttributes(LOAD_ID_4, 5.0);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(load1, load2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(load3, load4), List.of());

        return List.of(filter1, filter2);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(LoadField.ACTIVE_POWER.name(),
                List.of(filter1),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(LoadField.ACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(25.).build()
        );

        FormulaInfos formulaInfos2 = getFormulaInfo(LoadField.REACTIVE_POWER.name(),
                List.of(filter2),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(LoadField.REACTIVE_POWER.name()).build(),
                ReferenceFieldOrValue.builder().value(2.5).build()
        );
        return List.of(formulaInfos1, formulaInfos2);
    }

    @Override
    protected List<FormulaInfos> getUpdatedFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(LoadField.ACTIVE_POWER.name(),
                List.of(filter1),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(200.).build(),
                ReferenceFieldOrValue.builder().equipmentField(LoadField.ACTIVE_POWER.name()).build()
        );

        return List.of(formulaInfos1);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(125, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(105, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(175, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(375, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(80, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(70, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(150, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        // TODO later
    }
}
