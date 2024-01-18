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
public class HvdcLineByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String HVDC_LINE_ID_1 = "hvdcLine";

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
        assertNull(getNetwork().getHvdcLine(HVDC_LINE_ID_1));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getHvdcLine(HVDC_LINE_ID_1));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.HVDC_LINE;
    }

    @Override
    protected String getEquipmentNotFoundMessage() {
        return "Hvdc Line " + EQUIPMENT_WRONG_ID_1 + " not found";
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes hvdc1 = getIdentifiableAttributes(HVDC_LINE_ID_1);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(hvdc1), List.of());

        return List.of(filter1);
    }
}
