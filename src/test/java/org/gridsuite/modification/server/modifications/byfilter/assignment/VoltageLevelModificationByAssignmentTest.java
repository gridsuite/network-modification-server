/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuit;
import com.powsybl.iidm.network.extensions.IdentifiableShortCircuitAdder;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.VoltageLevelField;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;

import java.util.Date;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class VoltageLevelModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
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

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name())
            .value(10.)
            .filters(List.of(filter1, filter2))
            .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name())
            .value(120.)
            .filters(List.of(filter3))
            .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
            .editedField(VoltageLevelField.NOMINAL_VOLTAGE.name())
            .value(150.)
            .filters(List.of(filter4))
            .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.LOW_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(2.)
                .filters(List.of(filter5))
                .build();

        DoubleAssignmentInfos assignmentInfos5 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.HIGH_SHORT_CIRCUIT_CURRENT_LIMIT.name())
                .value(5.)
                .filters(List.of(filter4, filter5))
                .build();

        return List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4, assignmentInfos5);
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.LOW_VOLTAGE_LIMIT.name())
                .value(5.)
                .filters(List.of(filter1, filter2))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.HIGH_VOLTAGE_LIMIT.name())
                .value(1.5)
                .filters(List.of(filter3))
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .editedField(VoltageLevelField.NOMINAL_VOLTAGE.name())
                .value(150.)
                .filters(List.of(filter4))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3));

        return infosList;
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
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1).getLowVoltageLimit(), 0);
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2).getLowVoltageLimit(), 0);
        assertEquals(10, getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3).getLowVoltageLimit(), 0);

        VoltageLevel voltageLevel4 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit4 = voltageLevel4.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit4);
        assertEquals(10, voltageLevel4.getLowVoltageLimit(), 0);
        assertEquals(2, identifiableShortCircuit4.getIpMin(), 0);
        assertEquals(5, identifiableShortCircuit4.getIpMax(), 0);

        VoltageLevel voltageLevel5 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_5);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit5 = voltageLevel5.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit5);
        assertEquals(120, voltageLevel5.getHighVoltageLimit(), 0);
        assertEquals(150, voltageLevel5.getNominalV(), 0);
        assertEquals(5, identifiableShortCircuit5.getIpMax(), 0);

        VoltageLevel voltageLevel6 = getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_6);
        IdentifiableShortCircuit<VoltageLevel> identifiableShortCircuit6 = voltageLevel6.getExtension(IdentifiableShortCircuit.class);
        assertNotNull(identifiableShortCircuit6);
        assertEquals(120, voltageLevel6.getHighVoltageLimit(), 0);
        assertEquals(2, identifiableShortCircuit6.getIpMin(), 0);
        assertEquals(5, identifiableShortCircuit6.getIpMax(), 0);
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

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        // TODO later
    }
}
