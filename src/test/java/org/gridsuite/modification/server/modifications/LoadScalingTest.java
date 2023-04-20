/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLoadScalingInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.hamcrest.core.IsNull;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_SCALING_ERROR;
import static org.gridsuite.modification.server.service.FilterService.setFilterServerBaseUri;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScalingTest extends AbstractNetworkModificationTest {

    private static final UUID LOAD_SCALING_ID = UUID.randomUUID();

    private static final UUID FILTER_ID_1 = UUID.randomUUID();

    private static final UUID FILTER_ID_2 = UUID.randomUUID();

    private static final UUID FILTER_ID_3 = UUID.randomUUID();

    private static final UUID FILTER_ID_4 = UUID.randomUUID();

    private static final UUID FILTER_ID_5 = UUID.randomUUID();

    private static final UUID FILTER_NOT_FOUND_ID = UUID.randomUUID();

    private static final UUID FILTER_NO_DK = UUID.randomUUID();

    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();

    private static final UUID FILTER_WRONG_ID_2 = UUID.randomUUID();

    private static final String LOAD_ID_1 = "v1load";

    private static final String LOAD_ID_2 = "v5load";

    private static final String LOAD_ID_3 = "v6load";

    private static final String LOAD_ID_4 = "load4";

    private static final String LOAD_ID_5 = "load5";

    private static final String LOAD_ID_6 = "load6";

    private static final String LOAD_ID_7 = "load7";

    private static final String LOAD_ID_8 = "load8";

    private static final String LOAD_ID_9 = "load9";

    private static final String LOAD_ID_10 = "load10";

    public static final String LOAD_WRONG_ID_1 = "wrongId1";

    public static final String LOAD_WRONG_ID_2 = "wrongId2";

    private WireMockServer wireMock;

    @SneakyThrows
    @Before
    public void specificSetUp() {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        getNetwork().getLoad(LOAD_ID_1).setP0(100).setQ0(10);
        getNetwork().getLoad(LOAD_ID_2).setP0(200).setQ0(20);
        getNetwork().getLoad(LOAD_ID_3).setP0(200).setQ0(20);
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_4, LOAD_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_5, LOAD_ID_5, 20, 200, 2.0, "cn10", 12, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_6, LOAD_ID_6, 11, 120, 4.0, "cn10", 13, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v6"), LOAD_ID_7, LOAD_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_8, LOAD_ID_8, 10, 130, 3.0, "cn10", 15, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_9, LOAD_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v5"), LOAD_ID_10, LOAD_ID_10, 12, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP);

        wireMock = new WireMockServer(wireMockConfig().dynamicPort());
        wireMock.start();

        IdentifiableAttributes load1 = getIdentifiableAttributes(LOAD_ID_1, 1.0);
        IdentifiableAttributes load2 = getIdentifiableAttributes(LOAD_ID_2, 2.0);
        IdentifiableAttributes load3 = getIdentifiableAttributes(LOAD_ID_3, 2.0);
        IdentifiableAttributes load4 = getIdentifiableAttributes(LOAD_ID_4, 5.0);
        IdentifiableAttributes load5 = getIdentifiableAttributes(LOAD_ID_5, 6.0);
        IdentifiableAttributes load6 = getIdentifiableAttributes(LOAD_ID_6, 7.0);
        IdentifiableAttributes load7 = getIdentifiableAttributes(LOAD_ID_7, 3.0);
        IdentifiableAttributes load8 = getIdentifiableAttributes(LOAD_ID_8, 8.0);
        IdentifiableAttributes load9 = getIdentifiableAttributes(LOAD_ID_9, 0.0);
        IdentifiableAttributes load10 = getIdentifiableAttributes(LOAD_ID_10, 9.0);

        IdentifiableAttributes loadWrongId1 = getIdentifiableAttributes(LOAD_WRONG_ID_1, 2.0);
        IdentifiableAttributes loadWrongId2 = getIdentifiableAttributes(LOAD_WRONG_ID_2, 3.0);

        IdentifiableAttributes loadNoDK1 = getIdentifiableAttributes(LOAD_ID_2, null);
        IdentifiableAttributes loadNoDK2 = getIdentifiableAttributes(LOAD_ID_3, null);

        FilterEquipments filter1 = getFilterEquipments(FILTER_ID_1, "filter1", List.of(load1, load2), List.of());
        FilterEquipments filter2 = getFilterEquipments(FILTER_ID_2, "filter2", List.of(load3, load4), List.of());
        FilterEquipments filter3 = getFilterEquipments(FILTER_ID_3, "filter3", List.of(load5, load6), List.of());
        FilterEquipments filter4 = getFilterEquipments(FILTER_ID_4, "filter4", List.of(load7, load8), List.of());
        FilterEquipments filter5 = getFilterEquipments(FILTER_ID_5, "filter5", List.of(load9, load10), List.of());

        FilterEquipments wrongIdFilter1 = getFilterEquipments(FILTER_WRONG_ID_1, "wrongIdFilter1", List.of(loadWrongId1, loadWrongId2), List.of(LOAD_WRONG_ID_1, LOAD_WRONG_ID_2));
        FilterEquipments wrongIdFilter2 = getFilterEquipments(FILTER_WRONG_ID_2, "wrongIdFilter2", List.of(loadWrongId1, load10), List.of(LOAD_WRONG_ID_1));
        FilterEquipments noDistributionKeyFilter = getFilterEquipments(FILTER_NO_DK, "noDistributionKeyFilter", List.of(loadNoDK1, loadNoDK2), List.of());

        String path = "/v1/filters/export?networkUuid=" + getNetworkUuid() + "&variantId=variant_1&ids=";
        String pathRegex = "/v1/filters/export\\?networkUuid=" + getNetworkUuid() + "\\&variantId=variant_1\\&ids=";

        wireMock.stubFor(WireMock.get(WireMock.urlMatching(pathRegex + "(.+,){4}.*"))
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(filter1, filter2, filter3, filter4, filter5)))
                .withHeader("Content-Type", "application/json")));

        String params = "(" + FILTER_ID_5 + "|" + FILTER_WRONG_ID_2 + ")";
        wireMock.stubFor(WireMock.get(WireMock.urlMatching(pathRegex + params + "," + params))
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(wrongIdFilter2, filter5)))
                .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + FILTER_WRONG_ID_1)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(wrongIdFilter1)))
                .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + FILTER_NO_DK)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(noDistributionKeyFilter)))
                .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + FILTER_NOT_FOUND_ID)
            .willReturn(WireMock.notFound()
                .withHeader("Content-Type", "application/json")));

        setFilterServerBaseUri(wireMock.baseUrl());
    }

    @After
    public void shutDown() {
        wireMock.shutdown();
    }

    @SneakyThrows
    @Test
    public void testVentilationModeWithoutDistributionKey() {
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
            .uuid(LOAD_SCALING_ID)
            .date(ZonedDateTime.now())
            .variationType(VariationType.DELTA_P)
            .variations(List.of(variation1))
            .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationToCreate)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @SneakyThrows
    @Test
    public void testFilterWithWrongIds() {
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

        mockMvc.perform(post(getNetworkModificationUri())
                .content(mapper.writeValueAsString(loadScalingInfo))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LOAD_SCALING_ERROR, "All filters contains equipments with wrong ids").getMessage(),
                loadScalingInfo.getErrorType().name(), reportService);
    }

    @SneakyThrows
    @Test
    public void testScalingCreationWithWarning() {
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

        mockMvc.perform(post(getNetworkModificationUri())
                .content(mapper.writeValueAsString(loadScalingInfo))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isOk(),
                content().string(IsNull.notNullValue())
            );

        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
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
            .date(ZonedDateTime.now())
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
            .uuid(LOAD_SCALING_ID)
            .date(ZonedDateTime.now())
            .variationType(VariationType.TARGET_P)
            .variations(List.of(variation5))
            .build();
    }

    @Override
    protected MatcherLoadScalingInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadScalingInfos.createMatcherLoadScalingInfos((LoadScalingInfos) modificationInfos);
    }

    //TODO update values after PowSyBl release
    @Override
    protected void assertNetworkAfterCreation() {
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
    protected void assertNetworkAfterDeletion() {
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

    private IdentifiableAttributes getIdentifiableAttributes(String id, Double distributionKey) {
        return IdentifiableAttributes.builder()
            .id(id)
            .type(IdentifiableType.LOAD)
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
}
