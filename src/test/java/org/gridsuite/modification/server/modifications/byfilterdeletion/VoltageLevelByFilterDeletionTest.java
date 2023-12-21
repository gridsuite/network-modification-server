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
public class VoltageLevelByFilterDeletionTest extends AbstractByFilterDeletionTest {
    private static final String VOLTAGE_LEVEL_ID_1 = "v1";

    private static final String VOLTAGE_LEVEL_ID_2 = "v2";

    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";

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
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3));
        assertNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_1));
        assertNotNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_2));
        assertNotNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_3));
        assertNotNull(getNetwork().getVoltageLevel(VOLTAGE_LEVEL_ID_4));
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.VOLTAGE_LEVEL;
    }

    @Override
    protected String getEquipmentNotFoundMessage() {
        return "Voltage level not found: " + EQUIPMENT_WRONG_ID_1;
    }

    @Override
    protected List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes vl1 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_1);
        IdentifiableAttributes vl2 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_2);
        IdentifiableAttributes vl3 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_3);
        IdentifiableAttributes vl4 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_4);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(vl1, vl2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(vl3, vl4), List.of());

        return List.of(filter1, filter2);
    }
}
