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
public class EquipmentByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String LOAD_ID_1 = "load1";

    private static final String LOAD_ID_2 = "load2";

    private static final String LOAD_ID_3 = "load3";

    private static final String LOAD_ID_4 = "load4";

    @Before
    public void specificSetUp() {
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
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLoad(LOAD_ID_1));
        assertNotNull(getNetwork().getLoad(LOAD_ID_2));
        assertNotNull(getNetwork().getLoad(LOAD_ID_3));
        assertNotNull(getNetwork().getLoad(LOAD_ID_4));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.LOAD;
    }

    @Override
    protected String getEquipmentNotFoundMessage() {
        return "Connectable not found";
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes load1 = getIdentifiableAttributes(LOAD_ID_1);
        IdentifiableAttributes load2 = getIdentifiableAttributes(LOAD_ID_2);
        IdentifiableAttributes load3 = getIdentifiableAttributes(LOAD_ID_3);
        IdentifiableAttributes load4 = getIdentifiableAttributes(LOAD_ID_4);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(load1, load2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(load3, load4), List.of());

        return List.of(filter1, filter2);
    }
}
