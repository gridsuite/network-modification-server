/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.TopologyKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.VoltageLevelField;
import org.gridsuite.modification.server.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.byfilter.formula.Operator;
import org.gridsuite.modification.server.dto.byfilter.formula.ReferenceFieldOrValue;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.modifications.byfilter.AbstractByFilterModification.REPORT_KEY_BY_FILTER_MODIFICATION_SOME;
import static org.gridsuite.modification.server.modifications.byfilter.ByFormulaModification.REPORT_KEY_EQUIPMENT_MODIFIED_ERROR;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessageWithoutRank;
import static org.junit.Assert.*;

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
    private static final String VOLTAGE_LEVEL_ID_7 = "v7";

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

        getNetwork().getSubstation("s1").newVoltageLevel().setId(VOLTAGE_LEVEL_ID_7)
            .setTopologyKind(TopologyKind.NODE_BREAKER).setNominalV(380.).add();
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_4, 5.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_5, 6.0),
                new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_2, 2.0),
                new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_5, 6.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_4, 5.0),
                new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter6 = IdentifierListFilter.builder().id(FILTER_ID_6).modificationDate(new Date()).equipmentType(EquipmentType.VOLTAGE_LEVEL)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(VOLTAGE_LEVEL_ID_7, 5.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5, filter6);
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

        FormulaInfos formulaInfos6 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
            List.of(filter6),
            Operator.ADDITION,
            ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build(),
            ReferenceFieldOrValue.builder().value(10.).build()
        );

        FormulaInfos formulaInfos7 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
            List.of(filter6),
            Operator.ADDITION,
            ReferenceFieldOrValue.builder().value(100.).build(),
            ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name()).build()
        );

        FormulaInfos formulaInfos8 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
            List.of(filter6),
            Operator.ADDITION,
            ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name()).build(),
            ReferenceFieldOrValue.builder().value(100.).build()
        );

        FormulaInfos formulaInfos9 = getFormulaInfo(VoltageLevelField.LOW_VOLTAGE_LIMIT.name(),
            List.of(filter6),
            Operator.ADDITION,
            ReferenceFieldOrValue.builder().value(100.).build(),
            ReferenceFieldOrValue.builder().equipmentField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name()).build()
        );

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4, formulaInfos5,
            formulaInfos6, formulaInfos7, formulaInfos8, formulaInfos9);
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
    protected EquipmentType getEquipmentType() {
        return EquipmentType.VOLTAGE_LEVEL;
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

        assertTrue(Double.isNaN(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_7).getLowVoltageLimit()));
        assertTrue(Double.isNaN(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_7).getHighVoltageLimit()));
        assertLogMessageWithoutRank("Cannot modify equipment v7 : At least one of the value or referenced field is null", REPORT_KEY_EQUIPMENT_MODIFIED_ERROR, reportService);
        assertLogMessageWithoutRank("Some of the equipment have been modified : 14 equipment(s) modified and 4 equipment(s) not modified", REPORT_KEY_BY_FILTER_MODIFICATION_SOME, reportService);
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
