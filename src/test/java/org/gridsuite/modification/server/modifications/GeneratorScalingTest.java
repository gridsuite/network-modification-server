/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import lombok.SneakyThrows;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.nio.file.Paths;
import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
public class GeneratorScalingTest extends AbstractNetworkModificationTest {
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
    public static final String GENERATOR_WRONG_ID_1 = "wrongId1";
    public static final String PATH = "/v1/filters/metadata";

    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        createGenerators();
    }

    private void createGenerators() {
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

    private List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_4, 5.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_5, 6.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_7, 3.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_8, 8.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_9, 0.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_10, 9.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.GENERATOR));
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCreate();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);

        assertEquals(
            String.format("ScalingInfos(super=ModificationInfos(uuid=null, type=GENERATOR_SCALING, date=null, stashed=false, messageType=null, messageValues=null), variations=[ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter1)], variationMode=PROPORTIONAL_TO_PMAX, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter2)], variationMode=REGULAR_DISTRIBUTION, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter3)], variationMode=STACKING_UP, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter4)], variationMode=VENTILATION, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter1), FilterInfos(id=%s, name=filter5)], variationMode=PROPORTIONAL, variationValue=50.0, reactiveVariationMode=null)], variationType=DELTA_P)",
                FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_1, FILTER_ID_5),
            buildModification().toString()
        );
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    @Test
    public void testVentilationModeWithoutDistributionKey() throws Exception {
        IdentifierListFilter noDistributionKeyFilter = IdentifierListFilter.builder().id(FILTER_NO_DK).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_2, null),
                    new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_3, null)))
            .build();

        UUID subNoDk = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_NO_DK)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(noDistributionKeyFilter)))
                        .withHeader("Content-Type", "application/json"))).getId();

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
                .date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 0.01D);
        assertEquals(200, getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 0.01D);

        wireMockUtils.verifyGetRequest(subNoDk, PATH, handleQueryParams(FILTER_NO_DK), false);
    }

    @Test
    public void testFilterWithWrongIds() throws Exception {
        IdentifierListFilter wrongIdFilter1 = IdentifierListFilter.builder().id(FILTER_WRONG_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of())
            .build();

        UUID subWrongId = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_WRONG_ID_1)
                .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(wrongIdFilter1)))
                .withHeader("Content-Type", "application/json"))).getId();

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

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(generatorScalingInfo)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(generatorScalingInfo.getErrorType().name() + ": There is no valid equipment ID among the provided filter(s)",
                "invalidFilters", reportService);
        wireMockUtils.verifyGetRequest(subWrongId, PATH, handleQueryParams(FILTER_WRONG_ID_1), false);
    }

    @Test
    public void testScalingCreationWithWarning() throws Exception {
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_9, 0.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_10, 9.0)))
            .build();

        IdentifierListFilter wrongIdFilter2 = IdentifierListFilter.builder().id(FILTER_WRONG_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(GENERATOR_WRONG_ID_1, 2.0),
                new IdentifierListFilterEquipmentAttributes(GENERATOR_ID_10, 9.0)))
            .build();

        String params = "(" + FILTER_ID_5 + "|" + FILTER_WRONG_ID_2 + ")";
        UUID subFilter = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + params + "," + params))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(wrongIdFilter2, filter5)))
                        .withHeader("Content-Type", "application/json"))).getId();
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

        String modificationToCreateJson = mapper.writeValueAsString(generatorScalingInfo);

        var response = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        assertNotNull(response.getResponse().getContentAsString());
        assertEquals(600, getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 0.01D);
        assertEquals(300, getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 0.01D);

        wireMockUtils.verifyGetRequest(subFilter, PATH, Map.of("ids", WireMock.matching(".*")), false);
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

    private Map<String, StringValuePattern> handleQueryParams(UUID filterId) {
        return Map.of("ids", WireMock.equalTo(String.valueOf(filterId)));
    }

    private Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private String getPath(boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/metadata\\?ids=";
        }
        return "/v1/filters/metadata?ids=";
    }

    @Test
    public void testRegularDistributionAllConnected() {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of());
    }

    @Test
    public void testRegularDistributionOnlyGTH2Connected() {
        testVariationWithSomeDisconnections(VariationMode.REGULAR_DISTRIBUTION, List.of("GH1", "GH2", "GH3", "GTH1", "GTH3"));
    }

    @Test
    public void testAllModesGH1Disconnected() {
        for (VariationMode mode : VariationMode.values()) {
            testVariationWithSomeDisconnections(mode, List.of("GH1"));
        }
    }

    @SneakyThrows
    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> generatorsToDisconnect) {
        // use a dedicated network where we can easily disconnect generators
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some generators (must not be taken into account by the variation modification)
        generatorsToDisconnect.forEach(g -> getNetwork().getGenerator(g).getTerminal().disconnect());
        List<String> modifiedGenerators = Stream.of("GH1", "GH2", "GH3", "GTH1", "GTH2", "GTH3")
                .filter(g -> !generatorsToDisconnect.contains(g))
                .toList();

        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_ALL_GEN).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes("GH1", 0.0),
                    new IdentifierListFilterEquipmentAttributes("GH2", 100.0),
                    new IdentifierListFilterEquipmentAttributes("GH3", 100.0),
                    new IdentifierListFilterEquipmentAttributes("GTH1", 100.0),
                    new IdentifierListFilterEquipmentAttributes("GTH2", 100.0),
                    new IdentifierListFilterEquipmentAttributes("GTH3", 100.0)))
            .build();

        UUID subFilter = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_ALL_GEN)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter1)))
                        .withHeader("Content-Type", "application/json"))).getId();

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

        String modificationToCreateJson = mapper.writeValueAsString(generatorScalingInfo);
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // If we sum the targetP for all expected modified generators, we should have the requested variation value
        double connectedGeneratorsTargetP = modifiedGenerators
                .stream()
                .map(g -> getNetwork().getGenerator(g).getTargetP())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedGeneratorsTargetP, 0.001D);

        wireMockUtils.verifyGetRequest(subFilter, PATH, Map.of("ids", WireMock.matching(".*")), false);
    }
}
