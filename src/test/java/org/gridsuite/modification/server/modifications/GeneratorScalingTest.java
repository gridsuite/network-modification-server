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
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.NetworkModificationException.Type.GENERATOR_SCALING_ERROR;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
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
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();
    private static final String GENERATOR_ID_1 = "idGenerator";
    private static final String GENERATOR_ID_2 = "v5generator";
    private static final String GENERATOR_ID_3 = "v6generator";
    private static final String GENERATOR_ID_4 = "gen4";
    private static final String GENERATOR_ID_5 = "gen5";
    private static final String GENERATOR_ID_6 = "gen6";
    private static final String GENERATOR_ID_7 = "gen7";
    private static final String GENERATOR_ID_8 = "gen8";
    private static final String GENERATOR_ID_9 = "gen9";
    private static final String GENERATOR_ID_10 = "gen10";
    public static final String GENERATOR_WRONG_ID_1 = "wrongId1";
    public static final String GENERATOR_WRONG_ID_2 = "wrongId2";
    public static final String PATH = "/v1/filters/export";

    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        createGenerators();
    }

    // Add a test here to check for collection impact ?

    private void createGenerators() {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getGenerator(GENERATOR_ID_1).setTargetP(100).setMaxP(500);
        getNetwork().getGenerator(GENERATOR_ID_2).setTargetP(200).setMaxP(2000);
        getNetwork().getGenerator(GENERATOR_ID_3).setTargetP(200).setMaxP(2000);
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v2"), GENERATOR_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v6"), GENERATOR_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v3"), GENERATOR_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v4"), GENERATOR_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_10, 10, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP, 500, -1);
    }

    private List<FilterEquipments> getTestFilters() {
        IdentifiableAttributes gen1 = getIdentifiableAttributes(GENERATOR_ID_1, 1.0);
        IdentifiableAttributes gen2 = getIdentifiableAttributes(GENERATOR_ID_2, 2.0);
        IdentifiableAttributes gen3 = getIdentifiableAttributes(GENERATOR_ID_3, 2.0);
        IdentifiableAttributes gen4 = getIdentifiableAttributes(GENERATOR_ID_4, 5.0);
        IdentifiableAttributes gen5 = getIdentifiableAttributes(GENERATOR_ID_5, 6.0);
        IdentifiableAttributes gen6 = getIdentifiableAttributes(GENERATOR_ID_6, 7.0);
        IdentifiableAttributes gen7 = getIdentifiableAttributes(GENERATOR_ID_7, 3.0);
        IdentifiableAttributes gen8 = getIdentifiableAttributes(GENERATOR_ID_8, 8.0);
        IdentifiableAttributes gen9 = getIdentifiableAttributes(GENERATOR_ID_9, 0.0);
        IdentifiableAttributes gen10 = getIdentifiableAttributes(GENERATOR_ID_10, 9.0);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(gen1, gen2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(gen3, gen4), List.of());
        FilterEquipments filter3 = getFilterEquipments(FILTER_ID_3, "filter3", List.of(gen5, gen6), List.of());
        FilterEquipments filter4 = getFilterEquipments(FILTER_ID_4, "filter4", List.of(gen7, gen8), List.of());
        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(gen9, gen10), List.of());

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCreate();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);

        assertEquals(
            String.format("ScalingInfos(super=ModificationInfos(uuid=null, date=null), variations=[ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter1)], variationMode=PROPORTIONAL_TO_PMAX, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter2)], variationMode=REGULAR_DISTRIBUTION, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter3)], variationMode=STACKING_UP, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter4)], variationMode=VENTILATION, variationValue=50.0, reactiveVariationMode=null), ScalingVariationInfos(id=null, filters=[FilterInfos(id=%s, name=filter1), FilterInfos(id=%s, name=filter5)], variationMode=PROPORTIONAL, variationValue=50.0, reactiveVariationMode=null)], variationType=DELTA_P)",
                FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_1, FILTER_ID_5),
            buildModification().toString()
        );
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @Test
    public void testVentilationModeWithoutDistributionKey() throws Exception {
        IdentifiableAttributes genNoDK1 = getIdentifiableAttributes(GENERATOR_ID_2, null);
        IdentifiableAttributes genNoDK2 = getIdentifiableAttributes(GENERATOR_ID_3, null);
        FilterEquipments noDistributionKeyFilter = getFilterEquipments(FILTER_NO_DK, "noDistributionKeyFilter", List.of(genNoDK1, genNoDK2), List.of());

        UUID subNoDk = wireMockServer.stubFor(WireMock.get(getPath(getNetworkUuid(), false) + FILTER_NO_DK)
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

        wireMockUtils.verifyGetRequest(subNoDk, PATH, handleQueryParams(getNetworkUuid(), FILTER_NO_DK), false);
    }

    @Test
    public void testFilterWithWrongIds() throws Exception {

        IdentifiableAttributes genWrongId1 = getIdentifiableAttributes(GENERATOR_WRONG_ID_1, 2.0);
        IdentifiableAttributes genWrongId2 = getIdentifiableAttributes(GENERATOR_WRONG_ID_2, 3.0);

        FilterEquipments wrongIdFilter1 = getFilterEquipments(FILTER_WRONG_ID_1, "wrongIdFilter1", List.of(genWrongId1, genWrongId2), List.of(GENERATOR_WRONG_ID_1, GENERATOR_WRONG_ID_2));
        UUID subWrongId = wireMockServer.stubFor(WireMock.get(getPath(getNetworkUuid(), false) + FILTER_WRONG_ID_1)
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
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(generatorScalingInfo)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(GENERATOR_SCALING_ERROR, "All filters contains equipments with wrong ids").getMessage(),
                generatorScalingInfo.getErrorType().name(), reportService);
        wireMockUtils.verifyGetRequest(subWrongId, PATH, handleQueryParams(getNetworkUuid(), FILTER_WRONG_ID_1), false);
    }

    @Test
    public void testScalingCreationWithWarning() throws Exception {
        IdentifiableAttributes genWrongId1 = getIdentifiableAttributes(GENERATOR_WRONG_ID_1, 2.0);
        IdentifiableAttributes gen10 = getIdentifiableAttributes(GENERATOR_ID_10, 9.0);
        IdentifiableAttributes gen9 = getIdentifiableAttributes(GENERATOR_ID_9, 0.0);

        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(gen9, gen10), List.of());

        FilterEquipments wrongIdFilter2 = getFilterEquipments(FILTER_WRONG_ID_2, "wrongIdFilter2", List.of(genWrongId1, gen10), List.of(GENERATOR_WRONG_ID_1));

        String params = "(" + FILTER_ID_5 + "|" + FILTER_WRONG_ID_2 + ")";
        UUID subFilter = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + params + "," + params))
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

        wireMockUtils.verifyGetRequest(subFilter, PATH, Map.of("networkUuid", WireMock.equalTo(String.valueOf(getNetworkUuid())), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(".*")), false);
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
                .uuid(GENERATOR_SCALING_ID)
                //.date(ZonedDateTime.now().truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation5))
                .build();
    }

    @Override
    protected void assertNetworkAfterCreation() {
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
    protected void assertNetworkAfterDeletion() {
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

    private IdentifiableAttributes getIdentifiableAttributes(String id, Double distributionKey) {
        return IdentifiableAttributes.builder()
                .id(id)
                .type(IdentifiableType.GENERATOR)
                .distributionKey(distributionKey)
                .build();
    }

    private FilterEquipments getFilterEquipments(UUID filterID, String filterName, List<IdentifiableAttributes> identifiableAttributes, List<String> notFoundEquipments) {
        return FilterEquipments.builder()
                .filterId(filterID)
                .filterName(filterName)
                .identifiableAttributes(identifiableAttributes)
                .notFoundEquipments(notFoundEquipments)
                .build();
    }

    private Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, UUID filterId) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.equalTo(String.valueOf(filterId)));
    }

    private Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private String getPath(UUID networkUuid, boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=variant_1\\&ids=";
        }
        return "/v1/filters/export?networkUuid=" + networkUuid + "&variantId=variant_1&ids=";
    }
}
