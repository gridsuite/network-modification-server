/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.powsybl.iidm.network.IdentifiableType;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.LoadField;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;
import org.gridsuite.modification.server.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;

import java.util.Date;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createSubstationImpacts;
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
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_4, 5.0)))
            .build();

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
    protected EquipmentType getEquipmentType() {
        return EquipmentType.LOAD;
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
        // NOTE: this assert is based on the return of NetworkStoreListener#reduceNetworkImpacts()
        // since the test network has only 4 loads which are less than the collectionThreshold 5
        assertThat(impacts).containsAll(createSubstationImpacts(Set.of("s1", "s2")));
    }
}
