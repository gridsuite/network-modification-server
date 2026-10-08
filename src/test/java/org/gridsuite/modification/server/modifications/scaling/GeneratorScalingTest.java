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
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.GeneratorScalingInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.FilterWithDistributionKeysStub;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

import java.nio.file.Paths;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogNthMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
class GeneratorScalingTest extends AbstractScalingTest {
    private static final UUID GENERATOR_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
    private static final UUID FILTER_ID_ALL_GEN = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();
    private static final String GENERATOR_ID_1 = "gen1";
    private static final String GENERATOR_ID_2 = "gen2";
    private static final String GENERATOR_ID_3 = "gen3";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";
    private static final String GENERATOR_WRONG_ID_1 = "wrongId1";

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        //createGenerators
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getGenerator(GENERATOR_ID_1).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_2).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_3).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_4).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_5).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_6).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_7).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_8).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_9).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_10).setTargetP(100).setMaxP(500);
    }

    @Override
    protected List<FilterWithDistributionKeysStub> getTestFilters() {
        return List.of(
                createFilterStub(EquipmentType.GENERATOR, FILTER_ID_1, Map.of(GENERATOR_ID_1, 1.0, GENERATOR_ID_2, 2.0)),
                createFilterStub(EquipmentType.GENERATOR, FILTER_ID_2, Map.of(GENERATOR_ID_3, 2.0, GENERATOR_ID_4, 5.0)),
                createFilterStub(EquipmentType.GENERATOR, FILTER_ID_3, Map.of(GENERATOR_ID_5, 6.0, GENERATOR_ID_6, 7.0)),
                createFilterStub(EquipmentType.GENERATOR, FILTER_ID_4, Map.of(GENERATOR_ID_7, 3.0, GENERATOR_ID_8, 8.0)),
                createFilterStub(EquipmentType.GENERATOR, FILTER_ID_5, Map.of(GENERATOR_ID_9, 0.0, GENERATOR_ID_10, 9.0)),
                createFilterStub(EquipmentType.GENERATOR, FILTER_WRONG_ID_2, Map.of(GENERATOR_WRONG_ID_1, 2.0, GENERATOR_ID_10, 9.0)));
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.GENERATOR));
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(List.of(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5)));

        super.testCreate();

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);

        assertEquals(
            String.format("ScalingInfos(super=ModificationInfos(uuid=null, type=GENERATOR_SCALING, date=null, stashed=false, messageType=null, messageValues=null, activated=null, " +
                            "applicabilityByRootNetworkTag=null, " +
                            "description=null), variations=[ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter1)], " +
                            "variationMode=PROPORTIONAL_TO_PMAX, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, " +
                            "filters=[FilterInfos(id=%s, name=filter2)], variationMode=REGULAR_DISTRIBUTION, variationValue=50.0, reactiveVariationMode=null), " +
                            "ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter3)], variationMode=STACKING_UP, " +
                            "variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter4)], " +
                            "variationMode=VENTILATION, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, " +
                            "filters=[FilterInfos(id=%s, name=filter1), FilterInfos(id=%s, name=filter5)], " +
                            "variationMode=PROPORTIONAL, variationValue=50.0, reactiveVariationMode=null)], variationType=DELTA_P)",
                FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_1, FILTER_ID_5),
            buildModification().toString()
        );
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(List.of(List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5)));

        super.testCopy();

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);
    }

    @Test
    void testVentilationModeWithoutDistributionKey() throws Exception {
        Map<String, Double> distributionKeys = new LinkedHashMap<>();
        distributionKeys.put(GENERATOR_ID_2, null);
        distributionKeys.put(GENERATOR_ID_3, null);

        UUID subNoDk = stubFiltersWithDistributionKeys(List.of(createFilterStub(EquipmentType.GENERATOR, FILTER_NO_DK, distributionKeys)));

        var filter = FilterInfos.builder()
                .id(FILTER_NO_DK)
                .name("filter")
                .build();

        var variation1 = ScalingVariationInfos.builder()
                .variationValue(100D)
                .variationMode(VariationMode.VENTILATION)
                .filters(List.of(filter))
                .build();

        ModificationInfos modificationToCreate = GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                .date(Instant.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1))
                .build();

        String modificationToCreateJson = getJsonBody(modificationToCreate, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk());

        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
        assertLogMessage("Ventilation mode could not be applied: at least one equipment is missing a distribution key",
                "network.modification.distributionKeys.missingEquipmentKey", reportService);

        verifyFiltersWithDistributionKeysRequest(subNoDk, List.of(FILTER_NO_DK));
    }

    @Test
    void testFilterWithWrongIds() throws Exception {
        UUID subWrongId = stubFiltersWithDistributionKeys(List.of(createFilterStub(EquipmentType.GENERATOR, FILTER_WRONG_ID_1, Map.of())));

        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_1)
                .build();
        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();
        String body = getJsonBody(generatorScalingInfo, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("No equipment evaluated by filters", "network.modification.filterEvaluationResult.noResult", reportService);
        verifyFiltersWithDistributionKeysRequest(subWrongId, List.of(FILTER_WRONG_ID_1));
    }

    @Test
    void testScalingCreationWithWarning() throws Exception {
        List<StubbedFilterRequest> stubbedFilterRequests = stubScalingFilters(List.of(List.of(FILTER_WRONG_ID_2, FILTER_ID_5)));
        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_2)
                .build();

        var filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_ID_5)
                .build();

        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(900D)
                .filters(List.of(filter, filter2))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = getJsonBody(generatorScalingInfo, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        var response = mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk())
                .andReturn();

        assertNotNull(response.getResponse().getContentAsString());
        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);

        assertLogNthMessage("Evaluate filter filter", "network.modification.filterEvaluation", reportService, 1);
        assertLogNthMessage("Evaluate filter filter2", "network.modification.filterEvaluation", reportService, 2);
        assertLogMessage("Equipment " + GENERATOR_ID_10 + " already seen in previous filter evaluation, skipping it",
                "network.modification.filterEvaluation.equipmentAlreadySeen", reportService);
        assertLogMessage("2 equipment(s) evaluated by filters", "network.modification.filterEvaluationResult", reportService);

        verifyFiltersWithDistributionKeysRequests(stubbedFilterRequests);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createGeneratorsNetwork(networkUuid, new NetworkFactoryImpl());
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

        var variation1 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL_TO_PMAX)
                .variationValue(50D)
                .filters(List.of(filter1))
                .build();

        var variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .variationValue(50D)
                .filters(List.of(filter2))
                .build();

        var variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.STACKING_UP)
                .variationValue(50D)
                .filters(List.of(filter3))
                .build();

        var variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .variationValue(50D)
                .filters(List.of(filter4))
                .build();

        var variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter1, filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                //.date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1, variation2, variation3, variation4, variation5))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 3")
                .build();

        var variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .variationValue(50D)
                .filters(List.of(filter5))
                .build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                .uuid(GENERATOR_SCALING_ID)
                //.date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation5))
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertEquals(118.46, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0.01D);
        assertEquals(258.46, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(225, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);
        assertEquals(125, getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 0.01D);
        assertEquals(250, getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 0.01D);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 0.01D);
        assertEquals(213.63, getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 0.01D);
        assertEquals(136.36, getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 0.01D);
        assertEquals(215.38, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(107.69, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 0);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 0);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 0);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 0);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 0);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 0);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0);
        assertEquals(100, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0);
    }

    @Test
    void testRegularDistributionAllConnected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of());
    }

    @Test
    void testRegularDistributionOnlyGTH2Connected() throws Exception {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of("GH1", "GH2", "GH3", "GTH1", "GTH3"));
    }

    @Test
    void testAllModesGH1Disconnected() throws Exception {
        for (VariationMode mode : VariationMode.values()) {
            testVariationWithSomeDisconnections(mode, List.of("GH1"));
        }
    }

    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> generatorsToDisconnect) throws Exception {
        // use a dedicated network where we can easily disconnect generators
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some generators (must not be taken into account by the variation modification)
        generatorsToDisconnect.forEach(g -> getNetwork().getGenerator(g).getTerminal().disconnect());
        List<String> modifiedGenerators = Stream.of("GH1", "GH2", "GH3", "GTH1", "GTH2", "GTH3")
                .filter(g -> !generatorsToDisconnect.contains(g))
                .toList();

        UUID subFilter = stubFiltersWithDistributionKeys(List.of(createFilterStub(EquipmentType.GENERATOR, FILTER_ID_ALL_GEN, Map.of(
                "GH1", 0.0,
                "GH2", 100.0,
                "GH3", 100.0,
                "GTH1", 100.0,
                "GTH2", 100.0,
                "GTH3", 100.0))));

        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_ID_ALL_GEN)
                .build();
        final double variationValue = 100D;
        var variation = ScalingVariationInfos.builder()
                .variationMode(variationMode)
                .variationValue(variationValue)
                .filters(List.of(filter))
                .build();
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = getJsonBody(generatorScalingInfo, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk());

        // If we sum the targetP for all expected modified generators, we should have the requested variation value
        double connectedGeneratorsTargetP = modifiedGenerators
                .stream()
                .map(g -> getNetwork().getGenerator(g).getTargetP())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedGeneratorsTargetP, 0.001D);

        verifyFiltersWithDistributionKeysRequest(subFilter, List.of(FILTER_ID_ALL_GEN));
    }
}
