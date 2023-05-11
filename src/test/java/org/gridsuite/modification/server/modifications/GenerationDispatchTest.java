/*
  Copyright (c) 2023, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.GenerationDispatchInfos;
import org.gridsuite.modification.server.dto.GeneratorsFilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.MatcherGenerationDispatchInfos;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class GenerationDispatchTest extends AbstractNetworkModificationTest {
    private static final String GH1_ID = "GH1";
    private static final String GH2_ID = "GH2";
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String TEST1_ID = "TEST1";
    private static final String GROUP1_ID = "GROUP1";
    private static final String GROUP2_ID = "GROUP2";
    private static final String GROUP3_ID = "GROUP3";
    private static final String ABC_ID = "ABC";
    private static final String NEW_GROUP1_ID = "newGroup1";
    private static final String NEW_GROUP2_ID = "newGroup2";
    private static final String GEN1_NOT_FOUND_ID = "notFoundGen1";
    private static final String GEN2_NOT_FOUND_ID = "notFoundGen2";
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    public static final String PATH = "/v1/filters/export";

    @Autowired
    ApplicationContext context;

    @SneakyThrows
    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
    }

    private IdentifiableAttributes getIdentifiableAttributes(String id) {
        return IdentifiableAttributes.builder()
            .id(id)
            .type(IdentifiableType.GENERATOR)
            .build();
    }

    private FilterEquipments getFilterEquipments(UUID filterID, String filterName,
                                                 List<IdentifiableAttributes> identifiableAttributes,
                                                 List<String> notFoundEquipments) {
        return FilterEquipments.builder()
            .filterId(filterID)
            .filterName(filterName)
            .identifiableAttributes(identifiableAttributes)
            .notFoundEquipments(notFoundEquipments)
            .build();
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatch() {
        ModificationInfos modification = buildModification();

        // network with 2 synchronous components, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        String modificationJson = mapper.writeValueAsString(modification);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertNetworkAfterCreationWithStandardLossCoefficient();

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 138.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchWithHigherLossCoefficient() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(90.);

        // network with 2 synchronous components, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        String modificationJson = mapper.writeValueAsString(modification);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 836.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 446.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 380.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 70.0 MW", "SupplyDemandBalanceCouldNotBeMet" + secondSynchronousComponentNum, reportService);
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchWithInternalHvdc() {
        ModificationInfos modification = buildModification();

        // network with unique synchronous component, 2 internal hvdc lines and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatchInternalHvdc.xiidm", getClass().getResourceAsStream("/testGenerationDispatchInternalHvdc.xiidm")));

        String modificationJson = mapper.writeValueAsString(modification);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on unique synchronous component
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in the unique synchronous component
        assertLogMessage("The total demand is : 768.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 0.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 68.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchWithMaxPReduction() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                    GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));

        List<FilterEquipments> filters = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(GTH2_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
            getFilterEquipments(FILTER_ID_2, "filter2", List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID)), List.of()),
            getFilterEquipments(FILTER_ID_3, "filter3", List.of(getIdentifiableAttributes(GEN1_NOT_FOUND_ID), getIdentifiableAttributes(GEN2_NOT_FOUND_ID)), List.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID)));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + "(.+,){2}.*"))
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filters))
                .withHeader("Content-Type", "application/json"))).getId();

        String modificationJson = mapper.writeValueAsString(modification);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(76.5, getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(42.5, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(65.68, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 169.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchGeneratorsWithFixedSupply() {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_2).name("filter2").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_3).name("filter3").build()));
        ((GenerationDispatchInfos) modification).setGeneratorsWithFixedSupply(
            List.of(GeneratorsFilterInfos.builder().id(FILTER_ID_1).name("filter1").build(),
                GeneratorsFilterInfos.builder().id(FILTER_ID_4).name("filter4").build()));

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate, plannedOutageRate, predefinedActivePowerSetpoint defined for some generators
        setNetwork(Network.read("testGenerationDispatchFixedActivePower.xiidm", getClass().getResourceAsStream("/testGenerationDispatchFixedActivePower.xiidm")));

        List<FilterEquipments> filtersForPmaxReduction = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
            getFilterEquipments(FILTER_ID_2, "filter2", List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID)), List.of()),
            getFilterEquipments(FILTER_ID_3, "filter3", List.of(getIdentifiableAttributes(GEN1_NOT_FOUND_ID), getIdentifiableAttributes(GEN2_NOT_FOUND_ID)), List.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID)));
        UUID stubIdForPmaxReduction = wireMockServer.stubFor(WireMock.get(getPath(getNetworkUuid(), false) + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filtersForPmaxReduction))
                .withHeader("Content-Type", "application/json"))).getId();

        List<FilterEquipments> filtersForFixedSupply = List.of(getFilterEquipments(FILTER_ID_1, "filter1", List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID)), List.of()),
            getFilterEquipments(FILTER_ID_4, "filter4", List.of(getIdentifiableAttributes(TEST1_ID), getIdentifiableAttributes(GROUP2_ID)), List.of()));
        UUID stubIdForFixedSupply = wireMockServer.stubFor(WireMock.get(getPath(getNetworkUuid(), false) + FILTER_ID_1 + "," + FILTER_ID_4)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filtersForFixedSupply))
                .withHeader("Content-Type", "application/json"))).getId();

        String modificationJson = mapper.writeValueAsString(modification);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(90., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(0., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(65.68, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 60.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply exceeds the total demand", "TotalAmountFixedSupplyExceedsTotalDemand" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);

        wireMockUtils.verifyGetRequest(stubIdForPmaxReduction, PATH, handleQueryParams(getNetworkUuid(), filtersForPmaxReduction.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFixedSupply, PATH, handleQueryParams(getNetworkUuid(), filtersForFixedSupply.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @SneakyThrows
    @Test
    public void testGenerationDispatchErrorCheck() {
        GenerationDispatchInfos modification = GenerationDispatchInfos.builder().lossCoefficient(150.).defaultOutageRate(0.).build();
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));
        final GenerationDispatch generationDispatch1 = new GenerationDispatch(modification);
        assertThrows("GENERATION_DISPATCH_ERROR : The loss coefficient must be between 0 and 100", NetworkModificationException.class, () -> generationDispatch1.check(getNetwork()));

        modification = GenerationDispatchInfos.builder().lossCoefficient(20.).defaultOutageRate(140.).build();
        final GenerationDispatch generationDispatch2 = new GenerationDispatch(modification);
        assertThrows("GENERATION_DISPATCH_ERROR : The default outage rate must be between 0 and 100", NetworkModificationException.class, () -> generationDispatch2.check(getNetwork()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
    }

    @Override
    protected ModificationInfos buildModification() {
        return GenerationDispatchInfos.builder()
            .lossCoefficient(20.)
            .defaultOutageRate(0.)
            .generatorsWithoutOutage(List.of())
            .generatorsWithFixedSupply(List.of())
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return GenerationDispatchInfos.builder()
            .lossCoefficient(50.)
            .defaultOutageRate(25.)
            .generatorsWithoutOutage(List.of(GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name1").build()))
            .generatorsWithFixedSupply(List.of(GeneratorsFilterInfos.builder().id(UUID.randomUUID()).name("name2").build()))
            .build();
    }

    @Override
    protected MatcherGenerationDispatchInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGenerationDispatchInfos.createMatcherGenerationDispatchInfos((GenerationDispatchInfos) modificationInfos);
    }

    private void assertNetworkAfterCreationWithStandardLossCoefficient() {
        assertEquals(100., getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(70., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(130., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(150., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(30., getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNetworkAfterCreationWithStandardLossCoefficient();
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(85.357, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(50., getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(24.0, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(85.357, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);
    }

    private Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)),
                      "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private String getPath(UUID networkUuid, boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=InitialState\\&ids=";
        }
        return "/v1/filters/export?networkUuid=" + networkUuid + "&variantId=InitialState&ids=";
    }
}
