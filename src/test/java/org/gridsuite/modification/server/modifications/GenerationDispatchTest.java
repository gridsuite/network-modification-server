/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.modifications.byfilter.AbstractByFilterTest;
import org.gridsuite.modification.server.service.FilterService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import static org.gridsuite.modification.server.utils.TestUtils.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
class GenerationDispatchTest extends AbstractByFilterTest {
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String TEST1_ID = "TEST1";
    private static final String GROUP1_ID = "GROUP1";
    private static final String GROUP2_ID = "GROUP2";
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

    // generators of each filter, some of them not existing in the test networks
    private static final Map<UUID, Set<String>> GENERATORS_BY_FILTER = Map.of(
            FILTER_ID_1, Set.of(GTH2_ID, GROUP1_ID),
            FILTER_ID_2, Set.of(ABC_ID, GH3_ID),
            FILTER_ID_3, Set.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID),
            FILTER_ID_4, Set.of(GTH1_ID),
            FILTER_ID_5, Set.of(GTH2_ID, GH3_ID, GEN1_NOT_FOUND_ID),
            FILTER_ID_6, Set.of(TEST1_ID));

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
    }

    @Override
    protected Map<UUID, Set<String>> getFilterMapping() {
        return GENERATORS_BY_FILTER;
    }

    @Override
    protected IdentifiableType getIdentifiableType() {
        return IdentifiableType.GENERATOR;
    }

    @Override
    protected EquipmentType getEquipmentType() {
        return EquipmentType.GENERATOR;
    }

    private static FilterInfos filterInfos(UUID id, String name) {
        return FilterInfos.builder().id(id).name(name).build();
    }

    /**
     * Stubs the single standalone filters request made when building the modification, answering with the given filters.
     */
    private UUID stubGeneratorsFilters(Map<UUID, Set<String>> generatorsByFilter) {
        return stubStandaloneFilters(generatorsByFilter.entrySet().stream()
                .map(entry -> createFilterStub(entry.getKey(), entry.getValue()))
                .toList());
    }

    @Test
    void testGenerationDispatchWithMaxPReduction() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(
            List.of(filterInfos(FILTER_ID_1, "filter1"),
                    filterInfos(FILTER_ID_2, "filter2"),
                    filterInfos(FILTER_ID_3, "filter3")));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));

        Map<UUID, Set<String>> generatorsByFilter = Map.of(
                FILTER_ID_1, GENERATORS_BY_FILTER.get(FILTER_ID_1),
                FILTER_ID_2, GENERATORS_BY_FILTER.get(FILTER_ID_2),
                FILTER_ID_3, GENERATORS_BY_FILTER.get(FILTER_ID_3));
        UUID stubId = stubGeneratorsFilters(generatorsByFilter);

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        verifyStandaloneFiltersRequest(stubId, generatorsByFilter.keySet());
    }

    @Test
    void testGenerationDispatchGeneratorsWithFixedSupply() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(
            List.of(filterInfos(FILTER_ID_1, "filter1"),
                filterInfos(FILTER_ID_2, "filter2"),
                filterInfos(FILTER_ID_3, "filter3")));
        ((GenerationDispatchInfos) modification).setGeneratorsWithFixedSupply(
            List.of(filterInfos(FILTER_ID_1, "filter1"),
                filterInfos(FILTER_ID_4, "filter4")));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate, plannedOutageRate, predefinedActivePowerSetpoint defined for some generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        // filter 1 is used in both sections, it contains one generator missing in the network
        Map<UUID, Set<String>> generatorsByFilter = Map.of(
                FILTER_ID_1, Set.of(GTH1_ID, GROUP1_ID, GEN1_NOT_FOUND_ID),
                FILTER_ID_2, Set.of(ABC_ID, GH3_ID),
                FILTER_ID_3, Set.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID),
                FILTER_ID_4, Set.of(TEST1_ID, GROUP2_ID));
        UUID stubId = stubGeneratorsFilters(generatorsByFilter);

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        // all the filters are loaded at once
        verifyStandaloneFiltersRequest(stubId, generatorsByFilter.keySet());
    }

    private static List<FilterInfos> getGeneratorsFiltersInfosWithFilters123() {
        return List.of(filterInfos(FILTER_ID_1, "filter1"),
                filterInfos(FILTER_ID_2, "filter2"),
                filterInfos(FILTER_ID_3, "filter3"));
    }

    private static List<GeneratorsFrequencyReserveInfos> getGeneratorsFrequencyReserveInfosWithFilters456() {
        return List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                        .generatorsFilters(List.of(filterInfos(FILTER_ID_4, "filter4"),
                                filterInfos(FILTER_ID_5, "filter5"))).build(),
                GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                        .generatorsFilters(List.of(filterInfos(FILTER_ID_6, "filter6"))).build());
    }

    @Test
    void testGenerationDispatchWithFrequencyReserve() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(getGeneratorsFiltersInfosWithFilters123());
        ((GenerationDispatchInfos) modification).setGeneratorsFrequencyReserve(getGeneratorsFrequencyReserveInfosWithFilters456());

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));
        getNetwork().getGenerator("GH1").setMinP(20.);  // to test scaling parameter allowsGeneratorOutOfActivePowerLimits

        UUID stubId = stubGeneratorsFilters(GENERATORS_BY_FILTER);

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        verifyStandaloneFiltersRequest(stubId, GENERATORS_BY_FILTER.keySet());
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

        UUID stubId = stubGeneratorsFilters(GENERATORS_BY_FILTER);

        String modificationJson = getJsonBody(modification, null);
        MvcResult mvcResult = runRequestAsync(mockMvc, post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON), status().isOk());
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());
        assertEquals(NetworkModificationResult.ApplicationStatus.WITH_WARNINGS, extractApplicationStatus(networkModificationsResult).getFirst());

        verifyStandaloneFiltersRequest(stubId, GENERATORS_BY_FILTER.keySet());
    }

    @Test
    void testGenerationDispatchWithMissingFilter() throws Exception {
        GenerationDispatchInfos modification = (GenerationDispatchInfos) buildModification();
        modification.setGeneratorsWithoutOutage(List.of(filterInfos(FILTER_ID_1, "filter1"), filterInfos(FILTER_ID_NOT_FOUND, "filterNotFound")));

        // the filter server only knows filter 1
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlPathEqualTo(PATH))
                .withQueryParam("ids", havingExactlyIdsIgnoringOrder(List.of(FILTER_ID_1, FILTER_ID_NOT_FOUND)))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(Map.of(FILTER_ID_1, equipmentFilter(GENERATORS_BY_FILTER.get(FILTER_ID_1)))))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        String modificationJson = getJsonBody(modification, null);
        MvcResult mvcResult = runRequestAsync(mockMvc, post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON), status().isOk());
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, extractApplicationStatus(networkModificationsResult).size());
        assertEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, extractApplicationStatus(networkModificationsResult).getFirst());

        verifyStandaloneFiltersRequest(stubId, Set.of(FILTER_ID_1, FILTER_ID_NOT_FOUND));
    }

    @Test
    void testGetGenerationDispatchWithCheckFiltersExistence() throws Exception {
        ModificationInfos modification = GenerationDispatchInfos.builder()
            .stashed(false)
            .lossCoefficient(20.)
            .defaultOutageRate(0.)
            .generatorsWithoutOutage(List.of(filterInfos(FILTER_ID_1, "filter1"),
                    filterInfos(FILTER_ID_2, "filter2"),
                    filterInfos(FILTER_ID_3, "filter3"),
                    filterInfos(FILTER_ID_NOT_FOUND, "filterNotFound")))
            .generatorsWithFixedSupply(List.of(filterInfos(FILTER_ID_1, "filter1"),
                filterInfos(FILTER_ID_4, "filter4"),
                filterInfos(FILTER_ID_NOT_FOUND, "filterNotFound")))
            .generatorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(3.)
                        .generatorsFilters(List.of(filterInfos(FILTER_ID_4, "filter4"),
                                filterInfos(FILTER_ID_5, "filter5"),
                                filterInfos(FILTER_ID_NOT_FOUND, "filterNotFound"))).build(),
                GeneratorsFrequencyReserveInfos.builder().frequencyReserve(5.)
                        .generatorsFilters(List.of(filterInfos(FILTER_ID_6, "filter6"))).build()))
            .substationsGeneratorsOrdering(List.of())
            .build();

        UUID modificationUuid = saveModification(modification);
        // filterNotFound has been deleted from the directory since the modification was saved
        directoryFilterNames.remove(FILTER_ID_NOT_FOUND);

        MvcResult mvcResult = mockMvc.perform(get("/v1/network-modifications/" + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ModificationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertInstanceOf(GenerationDispatchInfos.class, receivedModification);
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
            .generatorsWithoutOutage(List.of(filterInfos(UUID.randomUUID(), "name1")))
            .generatorsWithFixedSupply(List.of(filterInfos(UUID.randomUUID(), "name2")))
            .generatorsFrequencyReserve(List.of(GeneratorsFrequencyReserveInfos.builder().frequencyReserve(0.02)
                                                .generatorsFilters(List.of(
                                                    filterInfos(UUID.randomUUID(), "name3"),
                                                    filterInfos(UUID.randomUUID(), "name4"))).build()))
            .substationsGeneratorsOrdering(List.of())
            .build();
    }
}
