/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.EnumAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.equipmentfield.LoadField;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;

import java.util.Date;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createSubstationImpacts;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.junit.Assert.assertEquals;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
public class LoadModificationByAssignmentTest extends AbstractModificationByAssignmentTest {
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
    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(LoadField.ACTIVE_POWER.name())
                .value(25.)
                .filters(List.of(filter1))
                .build();

        DoubleAssignmentInfos assignmentInfos2 = DoubleAssignmentInfos.builder()
                .editedField(LoadField.REACTIVE_POWER.name())
                .value(2.5)
                .filters(List.of(filter2))
                .build();

        EnumAssignmentInfos assignmentInfos3 = EnumAssignmentInfos.builder()
                .editedField(LoadField.LOAD_TYPE.name())
                .value(LoadType.AUXILIARY.name())
                .filters(List.of(filter1))
                .build();

        List<AssignmentInfos<?>> infosList = super.getAssignmentInfos();
        infosList.addAll(List.of(assignmentInfos1, assignmentInfos2, assignmentInfos3));

        return infosList;
    }

    @Override
    protected List<AssignmentInfos<?>> getUpdatedAssignmentInfos() {
        DoubleAssignmentInfos assignmentInfos1 = DoubleAssignmentInfos.builder()
                .editedField(LoadField.ACTIVE_POWER.name())
                .value(200.)
                .filters(List.of(filter1))
                .build();

        return List.of(assignmentInfos1);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(25, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(25, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(2.5, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(2.5, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
        assertThat(getNetwork().getLoad(LOAD_ID_1).getLoadType()).isEqualTo(LoadType.AUXILIARY);
        assertThat(getNetwork().getLoad(LOAD_ID_2).getLoadType()).isEqualTo(LoadType.AUXILIARY);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(100, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(80, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(70, getNetwork().getLoad(LOAD_ID_3).getQ0(), 0);
        assertEquals(150, getNetwork().getLoad(LOAD_ID_4).getQ0(), 0);
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
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        // NOTE: this assert is based on the return of NetworkStoreListener#reduceNetworkImpacts()
        // since the test network has only 4 loads which is less than the collectionThreshold 5
        assertThat(impacts).containsAll(createSubstationImpacts(Set.of("s1", "s2")));
    }

}
