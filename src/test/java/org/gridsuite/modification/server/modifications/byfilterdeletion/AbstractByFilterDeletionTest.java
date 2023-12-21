package org.gridsuite.modification.server.modifications.byfilterdeletion;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public abstract class AbstractByFilterDeletionTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final String EQUIPMENT_WRONG_ID_1 = "wrongId1";

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract String getEquipmentNotFoundMessage();

    protected abstract List<FilterEquipments> getTestFilters();

    public static final String PATH = "/v1/filters/export";

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
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();

        List<FilterEquipments> filters = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(EQUIPMENT_WRONG_ID_1)), List.of()));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+){1}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(byFilterDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertLogMessage(getEquipmentNotFoundMessage(),
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

    @Test
    public void testCreateAllFiltersWrong() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();

        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();

        List<FilterEquipments> filters = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(EQUIPMENT_WRONG_ID_1)), List.of(EQUIPMENT_WRONG_ID_1)));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid()) + "(.+){1}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(byFilterDeletionInfos)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("All of the following filters have equipments with wrong id : filter1",
                "allFiltersWrong", reportService);
        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    private String getPath(UUID networkUuid) {
        return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=variant_1\\&ids=";
    }

    protected Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    FilterEquipments getFilterEquipments(UUID filterID, String filterName, List<IdentifiableAttributes> identifiableAttributes, List<String> notFoundEquipments) {
        return FilterEquipments.builder()
                .filterId(filterID)
                .filterName(filterName)
                .identifiableAttributes(identifiableAttributes)
                .notFoundEquipments(notFoundEquipments)
                .build();
    }

    IdentifiableAttributes getIdentifiableAttributes(String id) {
        return IdentifiableAttributes.builder()
                .id(id)
                .type(getIdentifiableType())
                .build();
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
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1, filter2))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2 modified")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter2))
                .build();
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(getIdentifiableType().name(), createdValues.get("equipmentType"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(getIdentifiableType().name(), createdValues.get("equipmentType"));
    }
}
