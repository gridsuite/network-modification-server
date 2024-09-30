/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.extensions.ActivePowerControl;
import com.powsybl.iidm.network.extensions.ActivePowerControlAdder;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.BatteryField;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createBattery;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class BatteryModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String BATTERY_ID_1 = "v3Battery";
    private static final String BATTERY_ID_2 = "battery2";
    private static final String BATTERY_ID_3 = "battery3";
    private static final String BATTERY_ID_4 = "battery4";
    private static final String BATTERY_ID_5 = "battery5";
    private static final String BATTERY_ID_6 = "battery6";

    @Test
    public void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(BATTERY_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        DoubleAssignmentInfos assignmentInfos = DoubleAssignmentInfos.builder()
                .filters(List.of(filterWithOneWrongId))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .value(55.)
                .build();

        checkCreateWithWarning(List.of(assignmentInfos), List.of(identifiableAttributes, wrongIdAttributes));
        assertEquals(55, getNetwork().getBattery(BATTERY_ID_1).getTargetP(), 0);
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
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter2))
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(80.)
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(BatteryField.MINIMUM_ACTIVE_POWER.name())
                .value(30.)
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter5))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .value(75.)
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.REACTIVE_POWER_SET_POINT.name())
                .value(2.)
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter4))
                .editedField(BatteryField.DROOP.name())
                .value(2.)
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4, assignmentInfos5));

        return infosList;
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter1, filter2))
                .editedField(BatteryField.MAXIMUM_ACTIVE_POWER.name())
                .value(200.)
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(BatteryField.MINIMUM_ACTIVE_POWER.name())
                .value(35.)
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .filters(List.of(filter3))
                .editedField(BatteryField.ACTIVE_POWER_SET_POINT.name())
                .value(10.)
                .build();

        return List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_1).getMaxP(), 0);
        assertEquals(2, getNetwork().getBattery(BATTERY_ID_1).getTargetQ(), 0);
        ActivePowerControl activePowerControl1 = getNetwork().getBattery(BATTERY_ID_1).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl1);
        assertEquals(2, activePowerControl1.getDroop(), 0);

        assertEquals(80, getNetwork().getBattery(BATTERY_ID_2).getMaxP(), 0);
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_2).getTargetP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_3).getMaxP(), 0);
        assertEquals(75, getNetwork().getBattery(BATTERY_ID_3).getTargetP(), 0);
        assertEquals(80, getNetwork().getBattery(BATTERY_ID_4).getMaxP(), 0);

        assertEquals(30, getNetwork().getBattery(BATTERY_ID_5).getMinP(), 0);
        assertEquals(2, getNetwork().getBattery(BATTERY_ID_5).getTargetQ(), 0);
        ActivePowerControl activePowerControl5 = getNetwork().getBattery(BATTERY_ID_5).getExtension(ActivePowerControl.class);
        assertNotNull(activePowerControl5);
        assertEquals(2, activePowerControl5.getDroop(), 0);

        assertEquals(30, getNetwork().getBattery(BATTERY_ID_6).getMinP(), 0);
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
