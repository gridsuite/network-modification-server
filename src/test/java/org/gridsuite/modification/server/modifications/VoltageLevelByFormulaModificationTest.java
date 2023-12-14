/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.formula.equipmentfield.VoltageLevelField;

import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

public class VoltageLevelByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String VOLTAGE_LEVEL_ID_1 = "v1";
    private static final String VOLTAGE_LEVEL_ID_2 = "v2";
    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";
    private static final String VOLTAGE_LEVEL_ID_5 = "v5";
    private static final String VOLTAGE_LEVEL_ID_6 = "v6";

    @Override
    protected void createEquipments() {
        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).setNominalV(400)
                .setHighVoltageLimit(200)
                .setLowVoltageLimit(100)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(120).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).setNominalV(150)
                .setLowVoltageLimit(100)
                .setHighVoltageLimit(1000)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(120).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).setNominalV(70)
                .setLowVoltageLimit(50)
                .setHighVoltageLimit(250)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(50).withIpMax(150).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4).setNominalV(100)
                .setLowVoltageLimit(70)
                .setHighVoltageLimit(300)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(10).withIpMax(100).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5).setNominalV(210)
                .setLowVoltageLimit(10)
                .setHighVoltageLimit(500)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(25).withIpMax(75).add();

        getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6).setNominalV(750)
                .setHighVoltageLimit(1000)
                .setLowVoltageLimit(90)
                .newExtension(IdentifiableShortCircuitAdder.class).withIpMin(100).withIpMax(200).add();
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes voltageLevel1 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_1, 1.0);
        IdentifiableAttributes voltageLevel2 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_2, 2.0);
        IdentifiableAttributes voltageLevel3 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_3, 2.0);
        IdentifiableAttributes voltageLevel4 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_4, 5.0);
        IdentifiableAttributes voltageLevel5 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_5, 6.0);
        IdentifiableAttributes voltageLevel6 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_6, 7.0);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(voltageLevel1, voltageLevel2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(voltageLevel3, voltageLevel4), List.of());
        FilterEquipments filter3 = getFilterEquipments(FILTER_ID_3, "filter3", List.of(voltageLevel5, voltageLevel6), List.of());
        FilterEquipments filter4 = getFilterEquipments(FILTER_ID_4, "filter4", List.of(voltageLevel2, voltageLevel5), List.of());
        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(voltageLevel4, voltageLevel6), List.of());

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(10.).build()
        );

        FormulaInfos formulaInfos2 = getFormulaInfo(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build()
        );

        FormulaInfos formulaInfos3 = getFormulaInfo(VoltageLevelField.NOMINAL_VOLTAGE.name(),
                List.of(filter4),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(150.).build(),
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build()
        );

        FormulaInfos formulaInfos4 = getFormulaInfo(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name(),
                List.of(filter5),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build()
        );

        FormulaInfos formulaInfos5 = getFormulaInfo(VoltageLevelField.HIGH_SHORT_CIRCUIT_CURRENT_LIMIT.name(),
                List.of(filter4, filter5),
                Operator.SUBTRACTION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.HIGH_SHORT_CIRCUIT_CURRENT_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(5.).build()
        );

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4, formulaInfos5);
    }

    @Override
    protected List<FormulaInfos> getUpdatedFormulaInfos() {
        FormulaInfos formulaInfos1 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(5.).build()
        );

        FormulaInfos formulaInfos2 = getFormulaInfo(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name(),
                List.of(filter3),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name()).build(),
                ReferenceFieldOrValue.builder().value(1.5).build()
        );

        FormulaInfos formulaInfos3 = getFormulaInfo(VoltageLevelField.NOMINAL_VOLTAGE.name(),
                List.of(filter4),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(150.).build(),
                ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build()
        );

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.VOLTAGE_LEVEL;
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(110, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).getLowVoltageLimit(), 0);
        assertEquals(110, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).getLowVoltageLimit(), 0);
        assertEquals(60, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).getLowVoltageLimit(), 0);

        VoltageLevel voltageLevel4 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit4 = voltageLevel4.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit4);
        assertEquals(80, voltageLevel4.getLowVoltageLimit(), 0);
        assertEquals(5, identifiableShortCircuit4.getIpMin(), 0);
        assertEquals(95, identifiableShortCircuit4.getIpMax(), 0);

        VoltageLevel voltageLevel5 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit5 = voltageLevel5.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit5);
        assertEquals(1000, voltageLevel5.getHighVoltageLimit(), 0);
        assertEquals(15, voltageLevel5.getNominalV(), 0);
        assertEquals(70, identifiableShortCircuit5.getIpMax(), 0);

        VoltageLevel voltageLevel6 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit6 = voltageLevel6.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit6);
        assertEquals(2000, voltageLevel6.getHighVoltageLimit(), 0);
        assertEquals(50, identifiableShortCircuit6.getIpMin(), 0);
        assertEquals(195, identifiableShortCircuit6.getIpMax(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(100, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).getLowVoltageLimit(), 0);
        assertEquals(100, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).getLowVoltageLimit(), 0);
        assertEquals(250, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).getHighVoltageLimit(), 0);

        VoltageLevel voltageLevel4 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit4 = voltageLevel4.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit4);
        assertEquals(300, voltageLevel4.getHighVoltageLimit(), 0);
        assertEquals(10, identifiableShortCircuit4.getIpMin(), 0);
        assertEquals(100, identifiableShortCircuit4.getIpMax(), 0);

        VoltageLevel voltageLevel5 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit5 = voltageLevel5.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit5);
        assertEquals(500, voltageLevel5.getHighVoltageLimit(), 0);
        assertEquals(210, voltageLevel5.getNominalV(), 0);
        assertEquals(75, identifiableShortCircuit5.getIpMax(), 0);

        VoltageLevel voltageLevel6 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit6 = voltageLevel6.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit6);
        assertEquals(1000, voltageLevel6.getHighVoltageLimit(), 0);
        assertEquals(100, identifiableShortCircuit6.getIpMin(), 0);
        assertEquals(200, identifiableShortCircuit6.getIpMax(), 0);
    }
}
