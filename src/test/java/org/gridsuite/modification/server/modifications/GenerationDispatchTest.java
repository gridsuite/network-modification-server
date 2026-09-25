/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.service.FilterService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import java.util.stream.Collectors;
import static org.gridsuite.modification.server.utils.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
class GenerationDispatchTest extends AbstractNetworkModificationTest {
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String TEST1_ID = "TEST1";
    private static final String GROUP1_ID = "GROUP1";
    private static final String ABC_ID = "ABC";
    private static final String GEN1_NOT_FOUND_ID = "notFoundGen1";
    private static final String GEN2_NOT_FOUND_ID = "notFoundGen2";
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_6 = UUID.randomUUID();
    private static final UUID FILTER_ID_NOT_FOUND = UUID.randomUUID();
    private static final String PATH = "/v1/filters/metadata";

    @BeforeEach
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
    }

    private static IdentifierListFilterEquipmentAttributes getIdentifiableAttributes(String id) {
        return new IdentifierListFilterEquipmentAttributes(id, null);
    }

    private static AbstractFilter getFilter(UUID filterID, List<IdentifierListFilterEquipmentAttributes> identifierListFilterEquipmentAttributes) {
        return IdentifierListFilter.builder().id(filterID).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(identifierListFilterEquipmentAttributes)
            .build();
    }

    @Test
    void testGenerationDispatchWithMaxPReduction() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));

        List<AbstractFilter> filters = List.of(getFilter(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GROUP1_ID))),
            getFilter(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID))),
            getFilter(FILTER_ID_3, List.of(getIdentifiableAttributes(GEN1_NOT_FOUND_ID), getIdentifiableAttributes(GEN2_NOT_FOUND_ID))));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + "(.+,){2}.*"))
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filters))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false, 2);
    }

    private static List<GeneratorsFilterInfos> getGeneratorsFiltersInfosWithFilters123() {
        return List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build());
    }

    private static List<GeneratorsFrequencyReserveInfos> getGeneratorsFrequencyReserveInfosWithFilters456() {
        return List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build(),
                                GeneratorsFilterInfos.builder().id(FILTER_ID_5).name("filter5").build())).build(),
                GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_6).name("filter6").build())).build());
    }

    private static List<AbstractFilter> getGeneratorsWithoutOutageFilters123() {
        return List.of(getFilter(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GROUP1_ID))),
                getFilter(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID))),
                getFilter(FILTER_ID_3, List.of(getIdentifiableAttributes(GEN1_NOT_FOUND_ID), getIdentifiableAttributes(GEN2_NOT_FOUND_ID))));
    }

    private static List<AbstractFilter> getGeneratorsFrequencyReserveFilters45() {
        return List.of(getFilter(FILTER_ID_4, List.of(getIdentifiableAttributes(GTH1_ID))),
                getFilter(FILTER_ID_5, List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GH3_ID), getIdentifiableAttributes(GEN1_NOT_FOUND_ID))));
    }

    private static List<AbstractFilter> getGeneratorsFrequencyReserveFilter6() {
        return List.of(getFilter(FILTER_ID_6, List.of(getIdentifiableAttributes(TEST1_ID))));
    }

    private static List<AbstractFilter> getFilters123456() {
        return List.of(getFilter(FILTER_ID_1, List.of()), getFilter(FILTER_ID_2, List.of()), getFilter(FILTER_ID_3, List.of()),
            getFilter(FILTER_ID_4, List.of()), getFilter(FILTER_ID_5, List.of()), getFilter(FILTER_ID_6, List.of()));
    }

    @Test
    void testGenerationDispatchWithMaxValueLessThanMinP() throws Exception {
        ModificationInfos modification = GenerationDispatchInfos.builder()
                .lossCoefficient(20.)
                .defaultOutageRate(15.)
                .generatorsWithoutOutage(getGeneratorsFiltersInfosWithFilters123())
                .generatorsWithFixedSupply(List.of())
                .generatorsFrequencyReserve(getGeneratorsFrequencyReserveInfosWithFilters456())
                .substationsGeneratorsOrdering(List.of())
                .build();

        // dedicated case
        setNetwork(Network.read("fourSubstations_abattementIndispo_modifPmin.xiidm", getClass().getResourceAsStream("/fourSubstations_abattementIndispo_modifPmin.xiidm")));

        // Stub filters queries
        UUID stubIdForPmaxReduction = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(getGeneratorsWithoutOutageFilters123()))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
        UUID stubIdForFrequencyReserve1 = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_4 + "," + FILTER_ID_5)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(getGeneratorsFrequencyReserveFilters45()))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
        UUID stubIdForFrequencyReserve2 = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_6)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(getGeneratorsFrequencyReserveFilter6()))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
        UUID stubIdForGetFilters = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3 + "," + FILTER_ID_4 + "," + FILTER_ID_5 + "," + FILTER_ID_6)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(getFilters123456()))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        String modificationJson = getJsonBody(modification, null);
        MvcResult mvcResult = runRequestAsync(mockMvc, post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON), status().isOk());
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());
        assertEquals(NetworkModificationResult.ApplicationStatus.WITH_WARNINGS, extractApplicationStatus(networkModificationsResult).getFirst());

        wireMockUtils.verifyGetRequest(stubIdForGetFilters, PATH, handleQueryParams(getFilters123456().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForPmaxReduction, PATH, handleQueryParams(getGeneratorsWithoutOutageFilters123().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve1, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilters45().stream().map(AbstractFilter::getId).collect(Collectors.toList())),
                false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve2, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilter6().stream().map(AbstractFilter::getId).collect(Collectors.toList())),
                false);
    }

    @Test
    void testGetGenerationDispatchWithCheckFiltersExistence() throws Exception {
        ModificationInfos modification = GenerationDispatchInfos.builder()
            .stashed(false)
            .lossCoefficient(20.)
            .defaultOutageRate(0.)
            .generatorsWithoutOutage(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_NOT_FOUND).name("filterNotFound").build()))
            .generatorsWithFixedSupply(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_NOT_FOUND).name("filterNotFound").build()))
            .generatorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build(),
                                GeneratorsFilterInfos.builder().id(FILTER_ID_5).name("filter5").build(),
                                GeneratorsFilterInfos.builder().id(FILTER_ID_NOT_FOUND).name("filterNotFound").build())).build(),
                GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                        .generatorsFilters(List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_6).name("filter6").build())).build()))
            .substationsGeneratorsOrdering(List.of())
            .build();

        UUID modificationUuid = saveModification(modification);

        UUID stubIdForGetFilters = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3 + "," + FILTER_ID_NOT_FOUND + "," + FILTER_ID_4 + "," +
                FILTER_ID_5 + "," + FILTER_ID_6)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(getFilters123456()))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        MvcResult mvcResult = mockMvc.perform(get("/v1/network-modifications/" + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ModificationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertInstanceOf(GenerationDispatchInfos.class, receivedModification);

        wireMockUtils.verifyGetRequest(stubIdForGetFilters, PATH, handleQueryParams(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_NOT_FOUND, FILTER_ID_4, FILTER_ID_5, FILTER_ID_6)), false);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
    }

    @Override
    protected ModificationInfos buildModification() {
        return GenerationDispatchInfos.builder()
            .stashed(false)
            .lossCoefficient(20.)
            .defaultOutageRate(0.)
            .generatorsWithoutOutage(List.of())
            .generatorsWithFixedSupply(List.of())
            .generatorsFrequencyReserve(List.of())
            .substationsGeneratorsOrdering(List.of())
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GenerationDispatchInfos.builder()
            .stashed(false)
            .lossCoefficient(50.)
            .defaultOutageRate(25.)
            .generatorsWithoutOutage(List.of(GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name1").build()))
            .generatorsWithFixedSupply(List.of(GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name2").build()))
            .generatorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(0.02)
                                                .generatorsFilters(List.of(
                                                    GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name3").build(),
                                                    GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name4").build())).build()))
            .substationsGeneratorsOrdering(List.of())
            .build();
    }

    private static Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private static String getPath(boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/metadata\\?ids=";
        }
        return "/v1/filters/metadata?ids=";
    }
}
