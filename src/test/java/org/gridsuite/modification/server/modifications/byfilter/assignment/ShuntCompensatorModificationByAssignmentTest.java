/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.ShuntCompensator;
import com.powsybl.iidm.network.ShuntCompensatorLinearModel;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.IntegerAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.ShuntCompensatorField;
import org.junit.Test;

import java.util.Date;
import java.util.List;

import static org.gridsuite.modification.server.utils.NetworkUtil.createShuntCompensator;
import static org.junit.Assert.assertEquals;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class ShuntCompensatorModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
    private static final String SHUNT_COMPENSATOR_ID_1 = "v1shunt";
    private static final String SHUNT_COMPENSATOR_ID_2 = "v2shunt";
    private static final String SHUNT_COMPENSATOR_ID_3 = "v3shunt";
    private static final String SHUNT_COMPENSATOR_ID_4 = "v4shunt";
    private static final String SHUNT_COMPENSATOR_ID_5 = "v5shunt";

    @Test
    public void testCreateWithWarning() throws Exception {
        IdentifierListFilterEquipmentAttributes identifiableAttributes = getIdentifiableAttributes(SHUNT_COMPENSATOR_ID_1, 1.0);
        IdentifierListFilterEquipmentAttributes wrongIdAttributes = getIdentifiableAttributes("wrongId", 1.0);

        IntegerAssignmentInfos assignmentInfos = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .value(2)
                .filters(List.of(filterWithOneWrongId))
                .build();

        checkCreateWithWarning(List.of(assignmentInfos), List.of(identifiableAttributes, wrongIdAttributes));
        assertEquals(2, getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1).getMaximumSectionCount(), 0);
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
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_1, 1.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_2, 1.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_3, 1.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_4, 1.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.SHUNT_COMPENSATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SHUNT_COMPENSATOR_ID_5, 1.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        IntegerAssignmentInfos assignmentInfos1 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .value(8)
                .filters(List.of(filter1, filter2))
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.SECTION_COUNT.name())
                .value(2)
                .filters(List.of(filter3))
                .build();

        DoubleAssignmentInfos assignmentInfos3 = DoubleAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SUSCEPTANCE.name())
                .value(5.)
                .filters(List.of(filter4))
                .build();

        DoubleAssignmentInfos assignmentInfos4 = DoubleAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_Q_AT_NOMINAL_VOLTAGE.name())
                .value(10.)
                .filters(List.of(filter5))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3, assignmentInfos4));

        return infosList;
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        IntegerAssignmentInfos assignmentInfos1 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.MAXIMUM_SECTION_COUNT.name())
                .value(150)
                .filters(List.of(filter1, filter2))
                .build();

        IntegerAssignmentInfos assignmentInfos2 = IntegerAssignmentInfos.builder()
                .editedField(ShuntCompensatorField.SECTION_COUNT.name())
                .value(2)
                .filters(List.of(filter3))
                .build();
        return List.of(assignmentInfos1, assignmentInfos2);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        ShuntCompensator shuntCompensator1 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_1);
        assertEquals(8, shuntCompensator1.getMaximumSectionCount());
        assertEquals(1, shuntCompensator1.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator2 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_2);
        assertEquals(8, shuntCompensator2.getMaximumSectionCount());
        assertEquals(0.375, shuntCompensator2.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator3 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_3);
        assertEquals(6, shuntCompensator3.getMaximumSectionCount());
        assertEquals(3, shuntCompensator3.getModel(ShuntCompensatorLinearModel.class).getBPerSection(), 0);

        ShuntCompensator shuntCompensator4 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_4);
        assertEquals(10, shuntCompensator4.getSectionCount());

        ShuntCompensator shuntCompensator5 = getNetwork().getShuntCompensator(SHUNT_COMPENSATOR_ID_5);
        assertEquals(2, shuntCompensator5.getSectionCount());
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
