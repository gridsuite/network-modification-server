/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;

import java.util.Date;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

@Tag("IntegrationTest")
class SubstationByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String SUBSTATION_ID_1 = "s1";
    private static final String SUBSTATION_ID_2 = "s2";
    private static final String SUBSTATION_ID_3 = "s3";

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_1));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_2));
        assertNull(getNetwork().getSubstation(SUBSTATION_ID_3));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_1));
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_2));
        assertNotNull(getNetwork().getSubstation(SUBSTATION_ID_3));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.SUBSTATION;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.SUBSTATION;
    }

    @Override
    protected String getExistingId() {
        return SUBSTATION_ID_1;
    }

    @Override
    protected List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.SUBSTATION)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SUBSTATION_ID_1, null),
                new IdentifierListFilterEquipmentAttributes(SUBSTATION_ID_2, null)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.SUBSTATION)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(SUBSTATION_ID_3, null)))
            .build();
        return List.of(filter1, filter2);
    }
}
