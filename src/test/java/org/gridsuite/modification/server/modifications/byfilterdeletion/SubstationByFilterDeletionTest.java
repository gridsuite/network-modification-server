/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilterdeletion;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.jupiter.api.Tag;

import java.util.List;
import java.util.UUID;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

@Tag("IntegrationTest")
public class SubstationByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String SUBSTATION_ID_1 = "s1";

    private static final String SUBSTATION_ID_2 = "s2";

    private static final String SUBSTATION_ID_3 = "s3";

    @Before
    public void specificSetUp() {
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
    protected String getEquipmentNotFoundMessage() {
        return "Substation not found: " + EQUIPMENT_WRONG_ID_1;
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes substation1 = getIdentifiableAttributes(SUBSTATION_ID_1);
        IdentifiableAttributes substation2 = getIdentifiableAttributes(SUBSTATION_ID_2);
        IdentifiableAttributes substation3 = getIdentifiableAttributes(SUBSTATION_ID_3);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(substation1, substation2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(substation3), List.of());

        return List.of(filter1, filter2);
    }
}
