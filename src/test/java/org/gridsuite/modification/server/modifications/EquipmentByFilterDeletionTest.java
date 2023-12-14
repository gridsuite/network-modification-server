/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class EquipmentByFilterDeletionTest extends AbstractNetworkModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final String LOAD_ID_1 = "load1";

    private static final String LOAD_ID_2 = "load2";

    private static final String LOAD_ID_3 = "load3";

    private static final String LOAD_ID_4 = "load4";

    private static final String LOAD_ID_5 = "load5";

    private static final String LOAD_ID_6 = "load6";

    private static final String LOAD_ID_7 = "load7";

    private static final String LOAD_ID_8 = "load8";

    private static final String LOAD_ID_9 = "load9";

    private static final String LOAD_ID_10 = "load10";

    public static final String PATH = "/v1/filters/export";
    public static final String LOAD_WRONG_ID_1 = "wrongId1";

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
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter3")
                .build();

        var filter4 = FilterInfos.builder()
                .id(FILTER_ID_4)
                .name("filter4")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter5")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType("LOAD")
                .equipmentFilters(List.of(filter1, filter2, filter3, filter4, filter5))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 5 modified")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType("LOAD")
                .equipmentFilters(List.of(filter5))
                .build();
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCreate();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType("LOAD")
                .equipmentFilters(List.of(filter1))
                .build();

        List<FilterEquipments> filters = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(LOAD_WRONG_ID_1))));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+){1}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        // delete load (fail because the load is not found)
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(byFilterDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Connectable not found: wrongId1",
                byFilterDeletionInfos.getErrorType().name(), reportService);
        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + ".{2,}"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLoad(LOAD_ID_1));
        assertNull(getNetwork().getLoad(LOAD_ID_2));
        assertNull(getNetwork().getLoad(LOAD_ID_3));
        assertNull(getNetwork().getLoad(LOAD_ID_4));
        assertNull(getNetwork().getLoad(LOAD_ID_5));
        assertNull(getNetwork().getLoad(LOAD_ID_6));
        assertNull(getNetwork().getLoad(LOAD_ID_7));
        assertNull(getNetwork().getLoad(LOAD_ID_8));
        assertNull(getNetwork().getLoad(LOAD_ID_9));
        assertNull(getNetwork().getLoad(LOAD_ID_10));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLoad(LOAD_ID_1));
        assertNotNull(getNetwork().getLoad(LOAD_ID_2));
        assertNotNull(getNetwork().getLoad(LOAD_ID_3));
        assertNotNull(getNetwork().getLoad(LOAD_ID_4));
        assertNotNull(getNetwork().getLoad(LOAD_ID_5));
        assertNotNull(getNetwork().getLoad(LOAD_ID_6));
        assertNotNull(getNetwork().getLoad(LOAD_ID_7));
        assertNotNull(getNetwork().getLoad(LOAD_ID_8));
        assertNotNull(getNetwork().getLoad(LOAD_ID_9));
        assertNotNull(getNetwork().getLoad(LOAD_ID_10));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("LOAD", createdValues.get("equipmentType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("LOAD", createdValues.get("equipmentType"));
    }

    private String getPath(UUID networkUuid) {
        return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=variant_1\\&ids=";
    }

    private Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes load1 = getIdentifiableAttributes(LOAD_ID_1);
        IdentifiableAttributes load2 = getIdentifiableAttributes(LOAD_ID_2);
        IdentifiableAttributes load3 = getIdentifiableAttributes(LOAD_ID_3);
        IdentifiableAttributes load4 = getIdentifiableAttributes(LOAD_ID_4);
        IdentifiableAttributes load5 = getIdentifiableAttributes(LOAD_ID_5);
        IdentifiableAttributes load6 = getIdentifiableAttributes(LOAD_ID_6);
        IdentifiableAttributes load7 = getIdentifiableAttributes(LOAD_ID_7);
        IdentifiableAttributes load8 = getIdentifiableAttributes(LOAD_ID_8);
        IdentifiableAttributes load9 = getIdentifiableAttributes(LOAD_ID_9);
        IdentifiableAttributes load10 = getIdentifiableAttributes(LOAD_ID_10);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(load1, load2));
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(load3, load4));
        FilterEquipments filter3 = getFilterEquipments(FILTER_ID_3, "filter3", List.of(load5, load6));
        FilterEquipments filter4 = getFilterEquipments(FILTER_ID_4, "filter4", List.of(load7, load8));
        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(load9, load10));

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    private FilterEquipments getFilterEquipments(UUID filterID, String filterName, List<IdentifiableAttributes> identifiableAttributes) {
        return FilterEquipments.builder()
                .filterId(filterID)
                .filterName(filterName)
                .identifiableAttributes(identifiableAttributes)
                .build();
    }

    private IdentifiableAttributes getIdentifiableAttributes(String id) {
        return IdentifiableAttributes.builder()
                .id(id)
                .type(IdentifiableType.LOAD)
                .build();
    }
}
