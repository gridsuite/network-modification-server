/*
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.NetworkReductionInfos;
import org.gridsuite.modification.server.modifications.byfilter.AbstractByFilterTest;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author David Braquart <david.braquart_externe at rte-france.com>
 */
@Tag("IntegrationTest")
class NetworkReductionTest extends AbstractByFilterTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();

    private static final String VOLTAGE_LEVEL_ID_1 = "v1";
    private static final String VOLTAGE_LEVEL_ID_2 = "v2";
    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";

    private static final Map<UUID, Set<String>> FILTER_MAPPING = Map.of(
            FILTER_ID_1, Set.of(VOLTAGE_LEVEL_ID_1, VOLTAGE_LEVEL_ID_2),
            FILTER_ID_2, Set.of(VOLTAGE_LEVEL_ID_3, VOLTAGE_LEVEL_ID_4)
    );

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Override
    protected Map<UUID, Set<String>> getFilterMapping() {
        return FILTER_MAPPING;
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.VOLTAGE_LEVEL;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.VOLTAGE_LEVEL;
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(List.of(getFilterMapping().keySet().stream().toList()));

        super.testCreate();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(List.of(getFilterMapping().keySet().stream().toList()));

        super.testCopy();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        return NetworkReductionInfos.builder()
                .stashed(false)
                .filters(List.of(filter1, filter2))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2 modified")
                .build();

        return NetworkReductionInfos.builder()
                .stashed(false)
                .filters(List.of(filter2))
                .build();
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
}
