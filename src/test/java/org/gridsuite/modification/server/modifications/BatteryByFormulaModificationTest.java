/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.formula.equipmentfield.BatteryField;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createBattery;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
class BatteryByFormulaModificationTest extends AbstractByFormulaModificationTest {
    private static final String BATTERY_ID_1 = "v3Battery";
    private static final String BATTERY_ID_2 = "battery2";
    private static final String BATTERY_ID_3 = "battery3";
    private static final String BATTERY_ID_4 = "battery4";
    private static final String BATTERY_ID_5 = "battery5";
    private static final String BATTERY_ID_6 = "battery6";

    @Test
    void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(BATTERY_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        FormulaInfos formulaInfos = FormulaInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(55.).build())
                .operator(Operator.ADDITION)
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(20.).build())
                .build();

        checkCreateWithWarning(List.of(formulaInfos), List.of(identifiableAttributes, wrongIdAttributes));
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_1).getTargetP(), 0);
    }

    @Override
    protected void createEquipments() {
        getNetwork().getBattery(BATTERY_ID_1).setTargetP(100).setMaxP(500).setMinP(0).setTargetQ(80);
        getNetwork().getBattery(BATTERY_ID_1).newExtension(ActivePowerControlAdder.class).withDroop(1).add();

        createBattery(getNetwork().getVoltageLevel("v2"), BATTERY_ID_2, "v2Battery2", 20, 50, 2000, 200, 50);
        createBattery(getNetwork().getVoltageLevel("v3"), BATTERY_ID_3, "v3Battery3", 30, 70, 400, 300, 50);

        createBattery(getNetwork().getVoltageLevel("v4"), BATTERY_ID_4, "v4Battery4", 40, 25, 350, 70, 50);

        createBattery(getNetwork().getVoltageLevel("v5"), BATTERY_ID_5, "v5Battery5", 50, 50, 600, 55, 140);
        getNetwork().getBattery(BATTERY_ID_5).newExtension(ActivePowerControlAdder.class).withDroop(4).add();

        createBattery(getNetwork().getVoltageLevel("v6"), BATTERY_ID_6, "v6Battery6", 60, 200, 700, 250, 210);
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.BATTERY)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(BATTERY_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(BATTERY_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.BATTERY)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(BATTERY_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(BATTERY_ID_4, 5.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.BATTERY)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(BATTERY_ID_5, 6.0),
                new IdentifierListFilterEquipmentAttributes(BATTERY_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.BATTERY)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(BATTERY_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(BATTERY_ID_5, 6.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.BATTERY)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(BATTERY_ID_2, 2.0),
                new IdentifierListFilterEquipmentAttributes(BATTERY_ID_3, 3.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<FormulaInfos> getFormulaInfos() {
        ReferenceFieldOrValue maxActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MAXIMUM_ACTIVE_POWER.name()).build();
        ReferenceFieldOrValue minActivePowerRef = ReferenceFieldOrValue.builder().equipmentField(BatteryField.MINIMUM_ACTIVE_POWER.name()).build();

        FormulaInfos formulaInfos1 = getFormulaInfo(BatteryField.MAXIMUM_ACTIVE_POWER.name(),
                List.of(filter1, filter2),
                Operator.ADDITION,
                maxActivePowerRef,
                ReferenceFieldOrValue.builder().value(50.).build());

        FormulaInfos formulaInfos2 = getFormulaInfo(BatteryField.MINIMUM_ACTIVE_POWER.name(),
                List.of(filter3),
                Operator.PERCENTAGE,
                ReferenceFieldOrValue.builder().value(30.).build(),
                minActivePowerRef);

        FormulaInfos formulaInfos3 = getFormulaInfo(BatteryField.ACTIVE_POWER_SET_POINT.name(),
                List.of(filter5),
                Operator.SUBTRACTION,
                maxActivePowerRef,
                minActivePowerRef);

        FormulaInfos formulaInfos4 = getFormulaInfo(BatteryField.REACTIVE_POWER_SET_POINT.name(),
                List.of(filter4),
                Operator.DIVISION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.REACTIVE_POWER_SET_POINT.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        FormulaInfos formulaInfos5 = getFormulaInfo(BatteryField.DROOP.name(),
                List.of(filter4),
                Operator.MULTIPLICATION,
                ReferenceFieldOrValue.builder().equipmentField(BatteryField.DROOP.name()).build(),
                ReferenceFieldOrValue.builder().value(2.).build());

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3, formulaInfos4, formulaInfos5);
    }

    @Override
    protected List<FormulaInfos> getUpdatedFormulaInfos() {
        FormulaInfos formulaInfos1 = FormulaInfos.builder()
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(200.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(BatteryField.MAXIMUM_ACTIVE_POWER.name()).build())
                .operator(Operator.ADDITION)
                .filters(List.of(filter1, filter2))
                .build();

        FormulaInfos formulaInfos2 = FormulaInfos.builder()
                .editedField(BatteryField.MINIMUM_ACTIVE_POWER.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(35.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().equipmentField(BatteryField.MINIMUM_ACTIVE_POWER.name()).build())
                .operator(Operator.PERCENTAGE)
                .filters(List.of(filter3))
                .build();

        FormulaInfos formulaInfos3 = FormulaInfos.builder()
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .fieldOrValue1(ReferenceFieldOrValue.builder().equipmentField(BatteryField.ACTIVE_POWER_SET_POINT.name()).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(10.).build())
                .operator(Operator.ADDITION)
                .filters(List.of(filter5))
                .build();

        return List.of(formulaInfos1, formulaInfos2, formulaInfos3);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(550, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(40, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(2, activePowerControl1.getDroop(), 0);

        assertEquals(2050, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(2000, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(450, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(380, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(400, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(15, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(70, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(8, activePowerControl5.getDroop(), 0);

        assertEquals(60, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(500, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(1, activePowerControl1.getDroop(), 0);

        assertEquals(2000, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(200, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(400, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(300, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(350, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(50, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(140, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(4, activePowerControl5.getDroop(), 0);

        assertEquals(200, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.BATTERY;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.BATTERY;
    }
}
