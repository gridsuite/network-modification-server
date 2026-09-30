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
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.error.NetworkModificationException;
import org.gridsuite.modification.error.NetworkModificationExceptionType;
import org.gridsuite.modification.modifications.GenerationDispatch;
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
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
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
    private static final String GH1_ID = "GH1";
    private static final String GH2_ID = "GH2";
    private static final String GH3_ID = "GH3";
    private static final String GTH1_ID = "GTH1";
    private static final String GTH2_ID = "GTH2";
    private static final String BATTERY1_ID = "BATTERY1";
    private static final String BATTERY2_ID = "BATTERY2";
    private static final String BATTERY3_ID = "BATTERY3";
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
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_6 = UUID.randomUUID();
    private static final UUID FILTER_ID_NOT_FOUND = UUID.randomUUID();
    // filters metadata endpoint, only used by the server to flag the filters that do not exist anymore when reading a modification
    private static final String FILTERS_METADATA_PATH = "/v1/filters/metadata";

    // generators of each filter, some of them not existing in the test networks
    private static final Map<UUID, Set<String>> GENERATORS_BY_FILTER = Map.of(
            FILTER_ID_1, Set.of(GTH2_ID, GROUP1_ID),
            FILTER_ID_2, Set.of(ABC_ID, GH3_ID),
            FILTER_ID_3, Set.of(GEN1_NOT_FOUND_ID, GEN2_NOT_FOUND_ID),
            FILTER_ID_4, Set.of(GTH1_ID),
            FILTER_ID_5, Set.of(GTH2_ID, GH3_ID, GEN1_NOT_FOUND_ID),
            FILTER_ID_6, Set.of(TEST1_ID));

    @BeforeEach
    public void specificSetUp() {
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

    private static AbstractFilter getMetadataFilter(UUID filterID) {
        return IdentifierListFilter.builder().id(filterID).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of())
            .build();
    }

    private void assertLogReportsForDefaultNetwork(double batteryBalanceOnSc2) {
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The battery balance is : 0.0 MW", "network.modification.TotalActiveBatteryTargetP", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 138.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);
        // on SC 2, we have to substract the battery balance
        final double defaultTotalAmount = 330.0;
        DecimalFormat df = new DecimalFormat("#0.0", new DecimalFormatSymbols(Locale.US));
        final String totalAmount = df.format(defaultTotalAmount - batteryBalanceOnSc2);
        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The battery balance is : " + df.format(batteryBalanceOnSc2) + " MW", "network.modification.TotalActiveBatteryTargetP", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : " + totalAmount + " MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in SOUTH region: " + totalAmount + " MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: " + totalAmount +
                " MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "network.modification.SumGeneratorActivePower", reportService);
    }

    @Test
    void testGenerationDispatch() throws Exception {
        ModificationInfos modification = buildModification();

        // network with 2 synchronous components, no battery, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertNetworkAfterCreationWithStandardLossCoefficient();

        assertLogReportsForDefaultNetwork(0.);
    }

    @Test
    void testGenerationDispatchWithBattery() throws Exception {
        ModificationInfos modification = buildModification();

        // same than testGenerationDispatch, with 3 Batteries (in 2nd SC)
        setNetwork(Network.read("testGenerationDispatchWithBatteries.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithBatteries.xiidm")));
        // only 2 are connected
        assertTrue(getNetwork().getBattery(BATTERY1_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY2_ID).getTerminal().isConnected());
        assertFalse(getNetwork().getBattery(BATTERY3_ID).getTerminal().isConnected());
        final double batteryTotalTargetP = getNetwork().getBattery(BATTERY1_ID).getTargetP() + getNetwork().getBattery(BATTERY2_ID).getTargetP();

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertLogReportsForDefaultNetwork(batteryTotalTargetP);
    }

    @Test
    void testGenerationDispatchWithBatteryConnection() throws Exception {
        ModificationInfos modification = buildModification();

        // network with 3 Batteries (in 2nd SC)
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithBatteries.xiidm")));
        // connect the 3rd one
        assertTrue(getNetwork().getBattery(BATTERY1_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY2_ID).getTerminal().isConnected());
        assertFalse(getNetwork().getBattery(BATTERY3_ID).getTerminal().isConnected());
        assertTrue(getNetwork().getBattery(BATTERY3_ID).getTargetP() > 0);
        getNetwork().getBattery(BATTERY3_ID).getTerminal().connect();
        final double batteryTotalTargetP = getNetwork().getBattery(BATTERY1_ID).getTargetP() + getNetwork().getBattery(BATTERY2_ID).getTargetP() + getNetwork().getBattery(BATTERY3_ID).getTargetP();

        String modificationJson = getJsonBody(modification, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertLogReportsForDefaultNetwork(batteryTotalTargetP);
    }

    @Test
    void testGenerationDispatchWithMultipleEnergySource() throws Exception {
        ModificationInfos modification = buildModification();

        setNetwork(Network.read("testGenerationDispatchWithMultipleEnergySource.xiidm", getClass().getResourceAsStream("/testGenerationDispatchWithMultipleEnergySource.xiidm")));

        String modificationJson = getJsonBody(modification, null);
        runRequestAsync(mockMvc, post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON), status().isOk());

        assertLogMessageWithoutRank("The total demand is : 768.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 858.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("Marginal cost: 28.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank(
                "Sum of generator active power setpoints in SOUTH region: 858.0 MW (NUCLEAR: 150.0 MW, THERMAL: 200.0 MW, HYDRO: 108.0 MW, WIND AND SOLAR: 150.0 MW, OTHER: 250.0 MW).",
                        "network.modification.SumGeneratorActivePower", reportService);
    }

    @Test
    void testGenerationDispatchWithHigherLossCoefficient() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(90.);

        // network with 2 synchronous components, 2 hvdc lines between them and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm")));

        String modificationJson = getJsonBody(modification, null);
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
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 836.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 746.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 446.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 380.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 470.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could not be met : the remaining power imbalance is 70.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);
    }

    @Test
    void testGenerationDispatchWithInternalHvdc() throws Exception {
        ModificationInfos modification = buildModification();

        // network with unique synchronous component, 2 internal hvdc lines and no forcedOutageRate and plannedOutageRate for the generators
        setNetwork(Network.read("testGenerationDispatchInternalHvdc.xiidm", getClass().getResourceAsStream("/testGenerationDispatchInternalHvdc.xiidm")));

        String modificationJson = getJsonBody(modification, null);
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
        // GTH1 is in the unique synchronous component
        assertLogMessage("The total demand is : 768.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 0.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 768.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 68.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);
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
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 169.0 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).",
                "network.modification.SumGeneratorActivePower", reportService);
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
        setNetwork(Network.read("testGenerationDispatchFixedActivePower.xiidm", getClass().getResourceAsStream("/testGenerationDispatchFixedActivePower.xiidm")));

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

        // filters evaluation, grouped by usage
        assertLogMessage("Evaluate filters of generators without outage simulation",
                "network.modification.generationDispatch.filtersEvaluation.generatorsWithoutOutage", reportService);
        assertLogMessage("Evaluate filters of generators with fixed active power",
                "network.modification.generationDispatch.filtersEvaluation.generatorsWithFixedSupply", reportService);
        assertLogMessage("Partial match: 1 elements not found", "filter.evaluation.listFilter.notFound", reportService);
        assertLogMessage("notFoundGen1 not found", "filter.evaluation.listFilter.notFoundId", reportService);
        assertLogMessage("No matching equipment found in the network", "filter.evaluation.general.noMatchingEquipment", reportService);

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 60.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 90.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of fixed supply exceeds the total demand", "network.modification.TotalAmountFixedSupplyExceedsTotalDemand", reportService);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in EAST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).",
                "network.modification.SumGeneratorActivePower", reportService);

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

        assertEquals(74.82, getNetwork().getGenerator(GH1_ID).getTargetP(), 0.001);
        assertEquals(59.5, getNetwork().getGenerator(GH2_ID).getTargetP(), 0.001);
        assertEquals(126.1, getNetwork().getGenerator(GH3_ID).getTargetP(), 0.001);
        assertEquals(74.205, getNetwork().getGenerator(GTH1_ID).getTargetP(), 0.001);
        assertEquals(145.5, getNetwork().getGenerator(GTH2_ID).getTargetP(), 0.001);
        assertEquals(40.375, getNetwork().getGenerator(TEST1_ID).getTargetP(), 0.001);
        assertEquals(100., getNetwork().getGenerator(GROUP1_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(100., getNetwork().getGenerator(GROUP2_ID).getTargetP(), 0.001);  // not modified : disconnected
        assertEquals(0., getNetwork().getGenerator(GROUP3_ID).getTargetP(), 0.001);
        assertEquals(69.58, getNetwork().getGenerator(ABC_ID).getTargetP(), 0.001);
        assertEquals(5., getNetwork().getGenerator(NEW_GROUP1_ID).getTargetP(), 0.001);  // not modified : not in main connected component
        assertEquals(7., getNetwork().getGenerator(NEW_GROUP2_ID).getTargetP(), 0.001);  // not modified : not in main connected component

        // test total demand and remaining power imbalance on synchronous components
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 177.9 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);

        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).",
                "network.modification.SumGeneratorActivePower", reportService);

        verifyStandaloneFiltersRequest(stubId, GENERATORS_BY_FILTER.keySet());
    }

    @Test
    void testGenerationDispatchWithSubstationsHierarchy() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setLossCoefficient(10.);
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(20.);
        ((GenerationDispatchInfos) modification).setSubstationsGeneratorsOrdering(List.of(
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S5", "S4", "S54", "S15", "S74")).build(),
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S27")).build(),
            SubstationsGeneratorsOrderingInfos.builder().substationIds(List.of("S113", "S74")).build()));

        // network
        setNetwork(Network.read("ieee118cdf_testDemGroupe.xiidm", getClass().getResourceAsStream("/ieee118cdf_testDemGroupe.xiidm")));

        String modificationJson = getJsonBody(modification, null);
        runRequestAsync(mockMvc, post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON), status().isOk());

        // generators modified
        assertEquals(264, getNetwork().getGenerator("B4-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B8-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B15-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B19-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B24-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B25-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B27-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B40-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B42-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B46-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B49-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B54-G").getTargetP(), 0.001);
        assertEquals(74.8, getNetwork().getGenerator("B62-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B74-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("B113-G").getTargetP(), 0.001);
        assertEquals(264, getNetwork().getGenerator("Group3").getTargetP(), 0.001);

        // other generators set to 0.
        assertEquals(0, getNetwork().getGenerator("B1-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B6-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B10-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B12-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B18-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B26-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B31-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B32-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B34-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B36-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B55-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B56-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B59-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B61-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B65-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B66-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B69-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B70-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B72-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B73-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B76-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B77-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B80-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B85-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B87-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B89-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B90-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B91-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B92-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B99-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B100-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B103-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B104-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B105-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B107-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B110-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B111-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B112-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("B116-G").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("Group1").getTargetP(), 0.001);
        assertEquals(0, getNetwork().getGenerator("Group2").getTargetP(), 0.001);
    }

    @Test
    void testGenerationDispatchErrorCheck() {
        final Network network = Network.read("testGenerationDispatch.xiidm", getClass().getResourceAsStream("/testGenerationDispatch.xiidm"));
        setNetwork(network);

        final GenerationDispatch generationDispatch1 = GenerationDispatch.builder().lossCoefficient(150.).defaultOutageRate(0.).build();
        assertThrows(NetworkModificationException.class, () -> generationDispatch1.check(network),
                NetworkModificationExceptionType.GENERATION_DISPATCH_ERROR.getMessage() + " : The loss coefficient must be between 0 and 100");

        final GenerationDispatch generationDispatch2 = GenerationDispatch.builder().lossCoefficient(20.).defaultOutageRate(140.).build();
        assertThrows(NetworkModificationException.class, () -> generationDispatch2.check(network),
                NetworkModificationExceptionType.GENERATION_DISPATCH_ERROR.getMessage() + " : The default outage rate must be between 0 and 100");
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

        // check logs
        // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogNthMessage("The active power set point of generator TEST1 has been set to 40.4 MW", "network.modification.GeneratorSetTargetP", reportService, 1);
        assertLogNthMessage("The active power set point of generator GTH1 has been set to 80.0 MW", "network.modification.GeneratorSetTargetP", reportService, 2);
        assertLogNthMessage("The active power set point of generator GTH2 has been set to 146.0 MW", "network.modification.GeneratorSetTargetP", reportService, 3);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 171.6 MW", "network.modification.SupplyDemandBalanceCouldNotBeMet", reportService);
        // GH1 is in second synchronous component
        assertLogMessageWithoutRank("The total demand is : 240.0 MW", "network.modification.TotalDemand", reportService);
        assertLogMessageWithoutRank("The total amount of fixed supply is : 0.0 MW", "network.modification.TotalAmountFixedSupply", reportService);
        assertLogMessageWithoutRank("The HVDC balance is : -90.0 MW", "network.modification.TotalOutwardHvdcFlow", reportService);
        assertLogMessageWithoutRank("The total amount of supply to be dispatched is : 330.0 MW", "network.modification.TotalAmountSupplyToBeDispatched", reportService);
        assertLogMessageWithoutRank("The active power set point of generator GH1 has been set to 80.0 MW", "network.modification.GeneratorSetTargetP", reportService);
        assertLogMessageWithoutRank("The active power set point of generator GH2 has been set to 60.0 MW", "network.modification.GeneratorSetTargetP", reportService);
        assertLogMessageWithoutRank("The active power set point of generator GH3 has been set to 126.1 MW", "network.modification.GeneratorSetTargetP", reportService);
        assertLogMessageWithoutRank("The active power set point of generator ABC has been set to 63.9 MW", "network.modification.GeneratorSetTargetP", reportService);
        assertLogMessageWithoutRank("Marginal cost: 150.0", "network.modification.MaxUsedMarginalCost", reportService);
        assertLogMessageWithoutRank("The supply-demand balance could be met", "network.modification.SupplyDemandBalanceCouldBeMet", reportService);
        assertLogMessageWithoutRank("Sum of generator active power setpoints in NORTH region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).",
                "network.modification.SumGeneratorActivePower", reportService);

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
        assertEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, extractApplicationStatus(networkModificationsResult).getFirst());

        assertLogMessage("The modification points to at least 1 filter that does not exist anymore", "network.modification.missingFiltersInGenerationDispatch", reportService);
        // the dispatch is not applied
        assertAfterNetworkModificationDeletion();
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

        UUID stubIdForGetFilters = wireMockServer.stubFor(WireMock.get(getPath() + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3 + "," + FILTER_ID_NOT_FOUND + "," + FILTER_ID_4 + "," +
                FILTER_ID_5 + "," + FILTER_ID_6)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(Stream.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5, FILTER_ID_6)
                        .map(GenerationDispatchTest::getMetadataFilter).toList()))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        MvcResult mvcResult = mockMvc.perform(get("/v1/network-modifications/" + modificationUuid))
                .andExpect(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        ModificationInfos receivedModification = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertInstanceOf(GenerationDispatchInfos.class, receivedModification);
        GenerationDispatchInfos receivedGenerationDispatch = (GenerationDispatchInfos) receivedModification;
        assertEquals(4, receivedGenerationDispatch.getGeneratorsWithoutOutage().size());
        assertEquals(3, receivedGenerationDispatch.getGeneratorsWithFixedSupply().size());
        assertEquals(2, receivedGenerationDispatch.getGeneratorsFrequencyReserve().size());
        assertEquals(3, receivedGenerationDispatch.getGeneratorsFrequencyReserve().getFirst().getGeneratorsFilters().size());

        assertNull(receivedGenerationDispatch.getGeneratorsWithoutOutage().get(3).getName());
        assertNull(receivedGenerationDispatch.getGeneratorsWithFixedSupply().get(2).getName());
        assertNull(receivedGenerationDispatch.getGeneratorsFrequencyReserve().getFirst().getGeneratorsFilters().get(2).getName());

        wireMockUtils.verifyGetRequest(stubIdForGetFilters, FILTERS_METADATA_PATH,
                handleQueryParams(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_NOT_FOUND, FILTER_ID_4, FILTER_ID_5, FILTER_ID_6)), false);
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
    protected void assertAfterNetworkModificationCreation() {
        assertNetworkAfterCreationWithStandardLossCoefficient();
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
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

    private static Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private static String getPath() {
        return FILTERS_METADATA_PATH + "?ids=";
    }
}
