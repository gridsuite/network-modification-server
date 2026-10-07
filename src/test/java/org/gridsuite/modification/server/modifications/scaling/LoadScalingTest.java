/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.scaling;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.ReactiveVariationMode;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.scaling.LoadScalingInfos;
import org.gridsuite.modification.dto.scaling.ScalingVariationInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.FilterWithDistributionKeysStub;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.hamcrest.core.IsNull;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Tag("IntegrationTest")
class LoadScalingTest extends AbstractScalingTest {
    private static final UUID LOAD_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_ALL_LOADS = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();
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
    private static final String LOAD_WRONG_ID_1 = "wrongId1";

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        //createLoads
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getLoad(LOAD_ID_1).setP0(100).setQ0(10);
        getNetwork().getLoad(LOAD_ID_2).setP0(200).setQ0(20);
        getNetwork().getLoad(LOAD_ID_3).setP0(200).setQ0(20);
        getNetwork().getLoad(LOAD_ID_4).setP0(100).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_5).setP0(200).setQ0(2.0);
        getNetwork().getLoad(LOAD_ID_6).setP0(120).setQ0(4.0);
        getNetwork().getLoad(LOAD_ID_7).setP0(200).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_8).setP0(130).setQ0(3.0);
        getNetwork().getLoad(LOAD_ID_9).setP0(200).setQ0(1.0);
        getNetwork().getLoad(LOAD_ID_10).setP0(100).setQ0(1.0);
    }

    @Override
    protected List<FilterWithDistributionKeysStub> getTestFilters() {
        return List.of(
                createFilterStub(EquipmentType.LOAD, FILTER_ID_1, Map.of(LOAD_ID_1, 1.0, LOAD_ID_2, 2.0)),
                createFilterStub(EquipmentType.LOAD, FILTER_ID_2, Map.of(LOAD_ID_3, 2.0, LOAD_ID_4, 5.0)),
                createFilterStub(EquipmentType.LOAD, FILTER_ID_3, Map.of(LOAD_ID_5, 6.0, LOAD_ID_6, 7.0)),
                createFilterStub(EquipmentType.LOAD, FILTER_ID_4, Map.of(LOAD_ID_7, 3.0, LOAD_ID_8, 8.0)),
                createFilterStub(EquipmentType.LOAD, FILTER_ID_5, Map.of(LOAD_ID_9, 0.0, LOAD_ID_10, 9.0)),
                createFilterStub(EquipmentType.LOAD, FILTER_WRONG_ID_2, Map.of(LOAD_WRONG_ID_1, 2.0, LOAD_ID_10, 9.0)));
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.LOAD));
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(
                List.of(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5)));

        super.testCreate();

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(
                List.of(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5)));

        super.testCopy();

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);
    }

    @Test
    void testVentilationModeWithoutDistributionKey() throws Exception {
        Map<String, Double> noDistributionKeys = new LinkedHashMap<>();
        noDistributionKeys.put(LOAD_ID_2, null);
        noDistributionKeys.put(LOAD_ID_3, null);

        UUID stubNonDistributionKey = stubFiltersWithDistributionKeys(
                List.of(createFilterStub(EquipmentType.LOAD, FILTER_NO_DK, noDistributionKeys)));
        FilterInfos filter = FilterInfos.builder()
            .id(FILTER_NO_DK)
            .name("filter")
            .build();

        ScalingVariationInfos variation1 = ScalingVariationInfos.builder()
            .variationValue(100D)
            .variationMode(VariationMode.VENTILATION)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .filters(List.of(filter))
            .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
            .stashed(false)
            .uuid(LOAD_SCALING_ID)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .variationType(VariationType.DELTA_P)
            .variations(List.of(variation1))
            .build();
        String body = getJsonBody(modificationToCreate, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
            .andExpect(status().isOk());

        verifyFiltersWithDistributionKeysRequest(stubNonDistributionKey, List.of(FILTER_NO_DK));

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertLogMessage("Ventilation mode could not be applied: at least one equipment is missing a distribution key",
                "network.modification.distributionKeys.missingEquipmentKey", reportService);
    }

    @Test
    void testFilterWithWrongIds() throws Exception {
        FilterInfos filter = FilterInfos.builder()
            .name("filter")
            .id(FILTER_WRONG_ID_1)
            .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .variationValue(100D)
            .filters(List.of(filter))
            .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
            .variationType(VariationType.TARGET_P)
            .variations(List.of(variation))
            .build();
        UUID stubWithWrongId = stubFiltersWithDistributionKeys(
                List.of(createFilterStub(EquipmentType.LOAD, FILTER_WRONG_ID_1, Map.of())));
        String body = getJsonBody(loadScalingInfo, null);

        mockMvc.perform(post(getNetworkModificationUri())
                .content(body)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("No equipment evaluated by filters", "network.modification.filterEvaluationResult.noResult", reportService);
        verifyFiltersWithDistributionKeysRequest(stubWithWrongId, List.of(FILTER_WRONG_ID_1));
    }

    @Test
    void testScalingCreationWithWarning() throws Exception {
        FilterInfos filter = FilterInfos.builder()
            .name("filter")
            .id(FILTER_WRONG_ID_2)
            .build();

        FilterInfos filter2 = FilterInfos.builder()
            .name("filter2")
            .id(FILTER_ID_5)
            .build();

        ScalingVariationInfos variation = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .variationValue(900D)
            .filters(List.of(filter, filter2))
            .build();

        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
            .variationType(VariationType.TARGET_P)
            .variations(List.of(variation))
            .build();

        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(List.of(List.of(FILTER_WRONG_ID_2, FILTER_ID_5)));
        String body = getJsonBody(loadScalingInfo, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
            .content(body)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
            .andExpectAll(
                status().isOk(),
                content().string(IsNull.notNullValue())
            );

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);
        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);

        assertLogNthMessage("Evaluate filter filter", "network.modification.filterEvaluation", reportService, 1);
        assertLogNthMessage("Evaluate filter filter2", "network.modification.filterEvaluation", reportService, 2);
        assertLogMessage("Equipment " + LOAD_ID_10 + " already seen in previous filter evaluation, skipping it",
                "network.modification.filterEvaluation.equipmentAlreadySeen", reportService);
        assertLogMessage("2 equipment(s) evaluated by filters", "network.modification.filterEvaluationResult", reportService);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected ModificationInfos buildModification() {
        FilterInfos filter1 = FilterInfos.builder()
            .id(FILTER_ID_1)
            .name("filter1")
            .build();

        FilterInfos filter2 = FilterInfos.builder()
            .id(FILTER_ID_2)
            .name("filter2")
            .build();

        FilterInfos filter3 = FilterInfos.builder()
            .id(FILTER_ID_3)
            .name("filter3")
            .build();

        FilterInfos filter4 = FilterInfos.builder()
            .id(FILTER_ID_4)
            .name("filter4")
            .build();

        FilterInfos filter5 = FilterInfos.builder()
            .id(FILTER_ID_5)
            .name("filter5")
            .build();

        ScalingVariationInfos variation1 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.REGULAR_DISTRIBUTION)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter2))
            .build();

        ScalingVariationInfos variation2 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.VENTILATION)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter4))
            .build();

        ScalingVariationInfos variation3 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(50D)
            .filters(List.of(filter1, filter5))
            .build();

        ScalingVariationInfos variation4 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
            .variationValue(100D)
            .filters(List.of(filter3))
            .build();

        ScalingVariationInfos variation5 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.REGULAR_DISTRIBUTION)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .variationValue(50D)
            .filters(List.of(filter3))
            .build();

        return LoadScalingInfos.builder()
            .stashed(false)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .variationType(VariationType.DELTA_P)
            .variations(List.of(variation1, variation2, variation3, variation4, variation5))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        FilterInfos filter5 = FilterInfos.builder()
            .id(FILTER_ID_5)
            .name("filter 3")
            .build();

        ScalingVariationInfos variation5 = ScalingVariationInfos.builder()
            .variationMode(VariationMode.PROPORTIONAL)
            .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
            .variationValue(50D)
            .filters(List.of(filter5))
            .build();

        return LoadScalingInfos.builder()
            .stashed(false)
            .uuid(LOAD_SCALING_ID)
            .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
            .variationType(VariationType.TARGET_P)
            .variations(List.of(variation5))
            .build();
    }

    //TODO update values after PowSyBl release
    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(108.33, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(216.66, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(225.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(125.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(287.5, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(182.5, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(213.63, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(166.36, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
        assertEquals(216.66, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(108.33, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_5).getP0(), 0);
        assertEquals(120.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_7).getP0(), 0);
        assertEquals(130.0, getNetwork().getLoad(LOAD_ID_8).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_9).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_10).getP0(), 0);
    }

    @Test
    void testRegularDistributionAllConnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of());
    }

    @Test
    void testRegularDistributionOnlyLD6Connected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of("LD1", "LD2", "LD3", "LD4", "LD5"));
    }

    @Test
    void testProportionalAllConnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of());
    }

    @Test
    void testProportionalAndVentilationLD1Disconnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1"));
        testVariationWithSomeDisconnections(VariationMode.VENTILATION, List.of("LD1"));
    }

    @Test
    void testProportionalOnlyLD6Connected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1", "LD2", "LD3", "LD4", "LD5"));
    }

    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> loadsToDisconnect) throws Exception {
        // use a dedicated network where we can easily disconnect loads
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some loads (must not be taken into account by the variation modification)
        loadsToDisconnect.forEach(l -> getNetwork().getLoad(l).getTerminal().disconnect());
        List<String> modifiedLoads = Stream.of("LD1", "LD2", "LD3", "LD4", "LD5", "LD6")
                .filter(l -> !loadsToDisconnect.contains(l))
                .toList();

        Map<String, Double> distributionKeys = new LinkedHashMap<>();
        distributionKeys.put("LD1", 0.0);
        distributionKeys.put("LD2", 100.0);
        distributionKeys.put("LD3", 100.0);
        distributionKeys.put("LD4", 100.0);
        distributionKeys.put("LD5", 100.0);
        distributionKeys.put("LD6", 100.0);

        UUID subFilter = stubFiltersWithDistributionKeys(
                List.of(createFilterStub(EquipmentType.LOAD, FILTER_ID_ALL_LOADS, distributionKeys)));

        FilterInfos filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_ALL_LOADS)
                .build();
        final double variationValue = 100D;
        ScalingVariationInfos variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(variationValue)
                .filters(List.of(filter))
                .build();
        LoadScalingInfos loadScalingInfo = LoadScalingInfos.builder()
                .stashed(false)
                .uuid(LOAD_SCALING_ID)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = getJsonBody(loadScalingInfo, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk());

        // If we sum the P0 for all expected modified loads, we should have the requested variation value
        double connectedLoadsConstantP = modifiedLoads
                .stream()
                .map(g -> getNetwork().getLoad(g).getP0())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedLoadsConstantP, 0.001D);

        verifyFiltersWithDistributionKeysRequest(subFilter, List.of(FILTER_ID_ALL_LOADS));
    }
}
