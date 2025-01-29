/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Tag("IntegrationTest")
class EquipmentByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String LOAD_ID_1 = "load1";
    private static final String LOAD_ID_2 = "load2";
    private static final String LOAD_ID_3 = "load3";
    private static final String LOAD_ID_4 = "load4";
    private static final String LOAD_ID_7 = "load7";
    private static final String LOAD_ID_11 = "load11";
    private static final String LOAD_ID_12 = "load12";

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLoad(LOAD_ID_1));
        assertNull(getNetwork().getLoad(LOAD_ID_2));
        assertNull(getNetwork().getLoad(LOAD_ID_3));
        assertNull(getNetwork().getLoad(LOAD_ID_4));
        assertNull(getNetwork().getLoad(LOAD_ID_7));
        assertNull(getNetwork().getLoad(LOAD_ID_11));
        assertNull(getNetwork().getLoad(LOAD_ID_12));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLoad(LOAD_ID_1));
        assertNotNull(getNetwork().getLoad(LOAD_ID_2));
        assertNotNull(getNetwork().getLoad(LOAD_ID_3));
        assertNotNull(getNetwork().getLoad(LOAD_ID_4));
        assertNotNull(getNetwork().getLoad(LOAD_ID_7));
        assertNotNull(getNetwork().getLoad(LOAD_ID_11));
        assertNotNull(getNetwork().getLoad(LOAD_ID_12));
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
    protected String getExistingId() {
        return LOAD_ID_1;
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_1, null),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_2, null)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_3, null),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_4, null)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_7, null),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_11, null),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_12, null)))
            .build();
        return List.of(filter1, filter2, filter3);
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.SUBSTATION));
    }
}
