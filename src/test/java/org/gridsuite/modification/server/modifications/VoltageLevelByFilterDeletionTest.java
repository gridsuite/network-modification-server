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
public class VoltageLevelByFilterDeletionTest extends AbstractNetworkModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final String VOLTAGE_LEVEL_ID_1 = "v1";

    private static final String VOLTAGE_LEVEL_ID_2 = "v2";

    private static final String VOLTAGE_LEVEL_ID_3 = "v3";
    private static final String VOLTAGE_LEVEL_ID_4 = "v4";

    public static final String PATH = "/v1/filters/export";
    public static final String VOLTAGE_LEVEL_WRONG_ID_1 = "wrongId1";

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
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter2")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType("VOLTAGE_LEVEL")
                .filters(List.of(filter1, filter2))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2 modified")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType("VOLTAGE_LEVEL")
                .filters(List.of(filter5))
                .build();
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+,){1}.*"))
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
                .equipmentType("VOLTAGE_LEVEL")
                .filters(List.of(filter1))
                .build();

        List<FilterEquipments> filters = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(VOLTAGE_LEVEL_WRONG_ID_1))));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+){1}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(byFilterDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Voltage level not found: wrongId1",
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
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("VOLTAGE_LEVEL", createdValues.get("equipmentType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("VOLTAGE_LEVEL", createdValues.get("equipmentType"));
    }

    private String getPath(UUID networkUuid) {
        return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=variant_1\\&ids=";
    }

    private Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes vl1 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_1);
        IdentifiableAttributes vl2 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_2);
        IdentifiableAttributes vl3 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_3);
        IdentifiableAttributes vl4 = getIdentifiableAttributes(VOLTAGE_LEVEL_ID_4);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(vl1, vl2));
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(vl3, vl4));

        return List.of(filter1, filter2);
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
                .type(IdentifiableType.VOLTAGE_LEVEL)
                .build();
    }
}
