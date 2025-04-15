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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.modifications.GenerationDispatch;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
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

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Tag("IntegrationTest")
class GenerationDispatchTest extends AbstractNetworkModificationTest {
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

    private void assertLogReportsForDefaultNetwork(double batteryBalanceOnSc2) {
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The battery balance is : 0.0 MW", "TotalActiveBatteryTargetP" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 138.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);
        // on SC 2, we have to substract the battery balance
        final double defaultTotalAmount = 330.0;
        DecimalFormat df = new DecimalFormat("#0.0", new DecimalFormatSymbols(Locale.US));
        final String totalAmount = df.format(defaultTotalAmount - batteryBalanceOnSc2);
        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The battery balance is : " + df.format(batteryBalanceOnSc2) + " MW", "TotalActiveBatteryTargetP" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : " + totalAmount + " MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Marginal cost: 150.0", "MaxUsedMarginalCost" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in SOUTH region: " + totalAmount + " MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: " + totalAmount + " MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "SumGeneratorActivePowerSOUTH" + secondSynchronousComponentNum, reportService);
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
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        int synchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum();
        assertLogMessage("The total demand is : 768.0 MW", "TotalDemand" + synchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + synchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + synchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 858.0 MW", "TotalAmountSupplyToBeDispatched" + synchronousComponentNum, reportService);
        assertLogMessage("Marginal cost: 28.0", "MaxUsedMarginalCost" + synchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + synchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in SOUTH region: 858.0 MW (NUCLEAR: 150.0 MW, THERMAL: 200.0 MW, HYDRO: 108.0 MW, WIND AND SOLAR: 150.0 MW, OTHER: 250.0 MW).", "SumGeneratorActivePowerSOUTH" + synchronousComponentNum, reportService);
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
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 836.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 746.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 446.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 380.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 470.0 MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 70.0 MW", "SupplyDemandBalanceCouldNotBeMet" + secondSynchronousComponentNum, reportService);
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
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in the unique synchronous component
        assertLogMessage("The total demand is : 768.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 0.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 768.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 68.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);
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
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 169.0 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 330.0 MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Marginal cost: 150.0", "MaxUsedMarginalCost" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "SumGeneratorActivePowerWEST" + secondSynchronousComponentNum, reportService);
        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    @Test
    void testGenerationDispatchGeneratorsWithFixedSupply() throws Exception {
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

        List<AbstractFilter> filtersForPmaxReduction = List.of(getFilter(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID))),
            getFilter(FILTER_ID_2, List.of(getIdentifiableAttributes(ABC_ID), getIdentifiableAttributes(GH3_ID))),
            getFilter(FILTER_ID_3, List.of(getIdentifiableAttributes(GEN1_NOT_FOUND_ID), getIdentifiableAttributes(GEN2_NOT_FOUND_ID))));
        UUID stubIdForPmaxReduction = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_1 + "," + FILTER_ID_2 + "," + FILTER_ID_3)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filtersForPmaxReduction))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        List<AbstractFilter> filtersForFixedSupply = List.of(getFilter(FILTER_ID_1, List.of(getIdentifiableAttributes(GTH1_ID), getIdentifiableAttributes(GROUP1_ID), getIdentifiableAttributes(GEN1_NOT_FOUND_ID))),
            getFilter(FILTER_ID_4, List.of(getIdentifiableAttributes(TEST1_ID), getIdentifiableAttributes(GROUP2_ID))));
        UUID stubIdForFixedSupply = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_1 + "," + FILTER_ID_4)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(filtersForFixedSupply))
                .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

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

        assertLogMessage("Generators without outage simulation: Cannot find 2 generators in filter filter3", "filterGeneratorsNotFoundgeneratorsWithoutOutage", reportService);
        assertLogMessage("Generators without outage simulation: Cannot find generator notFoundGen1 in filter filter3", "generatorNotFoundnotFoundGen1generatorsWithoutOutage", reportService);
        assertLogMessage("Generators without outage simulation: Cannot find generator notFoundGen2 in filter filter3", "generatorNotFoundnotFoundGen2generatorsWithoutOutage", reportService);
        assertLogMessage("Generators with fixed active power: Cannot find 1 generators in filter filter1", "filterGeneratorsNotFoundgeneratorsWithFixedSupply", reportService);
        assertLogMessage("Generators with fixed active power: Cannot find generator notFoundGen1 in filter filter1", "generatorNotFoundnotFoundGen1generatorsWithFixedSupply", reportService);

        // test total demand and remaining power imbalance on synchronous components
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 60.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 90.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply exceeds the total demand", "TotalAmountFixedSupplyExceedsTotalDemand" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 330.0 MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Marginal cost: 150.0", "MaxUsedMarginalCost" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in EAST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "SumGeneratorActivePowerEAST" + secondSynchronousComponentNum, reportService);

        wireMockUtils.verifyGetRequest(stubIdForPmaxReduction, PATH, handleQueryParams(filtersForPmaxReduction.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFixedSupply, PATH, handleQueryParams(filtersForFixedSupply.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
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

    @Test
    void testGenerationDispatchWithFrequencyReserve() throws Exception {
        ModificationInfos modification = buildModification();
        ((GenerationDispatchInfos) modification).setDefaultOutageRate(15.);
        ((GenerationDispatchInfos) modification).setGeneratorsWithoutOutage(getGeneratorsFiltersInfosWithFilters123());
        ((GenerationDispatchInfos) modification).setGeneratorsFrequencyReserve(getGeneratorsFrequencyReserveInfosWithFilters456());

        // network with 2 synchronous components, 2 hvdc lines between them, forcedOutageRate and plannedOutageRate defined for the generators
        setNetwork(Network.read("testGenerationDispatchReduceMaxP.xiidm", getClass().getResourceAsStream("/testGenerationDispatchReduceMaxP.xiidm")));
        getNetwork().getGenerator("GH1").setMinP(20.);  // to test scaling parameter allowsGeneratorOutOfActivePowerLimits

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
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 177.9 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);

        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 330.0 MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Marginal cost: 150.0", "MaxUsedMarginalCost" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in WEST region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "SumGeneratorActivePowerWEST" + secondSynchronousComponentNum, reportService);

        wireMockUtils.verifyGetRequest(stubIdForPmaxReduction, PATH, handleQueryParams(getGeneratorsWithoutOutageFilters123().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve1, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilters45().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve2, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilter6().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
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
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

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

        GenerationDispatchInfos modification = GenerationDispatchInfos.builder().lossCoefficient(150.).defaultOutageRate(0.).build();
        final GenerationDispatch generationDispatch1 = new GenerationDispatch(modification);
        assertThrows(NetworkModificationException.class, () -> generationDispatch1.check(network), "GENERATION_DISPATCH_ERROR : The loss coefficient must be between 0 and 100");

        modification = GenerationDispatchInfos.builder().lossCoefficient(20.).defaultOutageRate(140.).build();
        final GenerationDispatch generationDispatch2 = new GenerationDispatch(modification);
        assertThrows(NetworkModificationException.class, () -> generationDispatch2.check(network), "GENERATION_DISPATCH_ERROR : The default outage rate must be between 0 and 100");
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

        String modificationJson = getJsonBody(modification, null);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        Optional<NetworkModificationsResult> networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());
        assertEquals(1, extractApplicationStatus(networkModificationsResult.get()).size());
        assertEquals(NetworkModificationResult.ApplicationStatus.WITH_WARNINGS, extractApplicationStatus(networkModificationsResult.get()).getFirst());

        // check logs
        int firstSynchronousComponentNum = getNetwork().getGenerator(GTH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GTH1 is in first synchronous component
        assertLogMessage("The total demand is : 528.0 MW", "TotalDemand" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : 90.0 MW", "TotalOutwardHvdcFlow" + firstSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 438.0 MW", "TotalAmountSupplyToBeDispatched" + firstSynchronousComponentNum, reportService);
        assertLogNthMessage("The active power set point of generator TEST1 has been set to 40.4 MW", "GeneratorSetTargetP" + firstSynchronousComponentNum, reportService, 1);
        assertLogNthMessage("The active power set point of generator GTH1 has been set to 80.0 MW", "GeneratorSetTargetP" + firstSynchronousComponentNum, reportService, 2);
        assertLogNthMessage("The active power set point of generator GTH2 has been set to 146.0 MW", "GeneratorSetTargetP" + firstSynchronousComponentNum, reportService, 3);
        assertLogMessage("The supply-demand balance could not be met : the remaining power imbalance is 171.6 MW", "SupplyDemandBalanceCouldNotBeMet" + firstSynchronousComponentNum, reportService);
        int secondSynchronousComponentNum = getNetwork().getGenerator(GH1_ID).getTerminal().getBusView().getBus().getSynchronousComponent().getNum(); // GH1 is in second synchronous component
        assertLogMessage("The total demand is : 240.0 MW", "TotalDemand" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of fixed supply is : 0.0 MW", "TotalAmountFixedSupply" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The HVDC balance is : -90.0 MW", "TotalOutwardHvdcFlow" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The total amount of supply to be dispatched is : 330.0 MW", "TotalAmountSupplyToBeDispatched" + secondSynchronousComponentNum, reportService);
        assertLogNthMessage("The active power set point of generator GH1 has been set to 80.0 MW", "GeneratorSetTargetP" + secondSynchronousComponentNum, reportService, 1);
        assertLogNthMessage("The active power set point of generator GH2 has been set to 60.0 MW", "GeneratorSetTargetP" + secondSynchronousComponentNum, reportService, 2);
        assertLogNthMessage("The active power set point of generator GH3 has been set to 126.1 MW", "GeneratorSetTargetP" + secondSynchronousComponentNum, reportService, 3);
        assertLogNthMessage("The active power set point of generator ABC has been set to 63.9 MW", "GeneratorSetTargetP" + secondSynchronousComponentNum, reportService, 4);
        assertLogMessage("Marginal cost: 150.0", "MaxUsedMarginalCost" + secondSynchronousComponentNum, reportService);
        assertLogMessage("The supply-demand balance could be met", "SupplyDemandBalanceCouldBeMet" + secondSynchronousComponentNum, reportService);
        assertLogMessage("Sum of generator active power setpoints in NORTH region: 330.0 MW (NUCLEAR: 0.0 MW, THERMAL: 0.0 MW, HYDRO: 330.0 MW, WIND AND SOLAR: 0.0 MW, OTHER: 0.0 MW).", "SumGeneratorActivePowerNORTH" + secondSynchronousComponentNum, reportService);

        wireMockUtils.verifyGetRequest(stubIdForPmaxReduction, PATH, handleQueryParams(getGeneratorsWithoutOutageFilters123().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve1, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilters45().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
        wireMockUtils.verifyGetRequest(stubIdForFrequencyReserve2, PATH, handleQueryParams(getGeneratorsFrequencyReserveFilter6().stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
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

    private static String getPath(boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/metadata\\?ids=";
        }
        return "/v1/filters/metadata?ids=";
    }
}
