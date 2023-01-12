/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.MatcherLoadScalingInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScalingModificationTest extends AbstractNetworkModificationTest {

    private static final UUID FILTER_ID_1 = UUID.fromString("bdefd63f-6cd8-4686-b57b-6bc7aaffa202");
    private static final UUID FILTER_ID_2 = UUID.fromString("bdfad63f-6fe6-4686-b57b-6bc7aa11a202");
    private static final UUID FILTER_ID_3 = UUID.fromString("00bd063f-611f-4686-b57b-6bc7aa00a202");
    private static final UUID FILTER_ID_4 = UUID.fromString("6f11d63f-6f06-4686-b57b-6bc7aa66a202");
    private static final UUID FILTER_ID_5 = UUID.fromString("7100163f-60f1-4686-b57b-6bc7aa77a202");

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
    private static final UUID LOAD_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_NOT_FOUND_ID = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.fromString("7700ff56-60f1-4686-b57b-6bc7aa77ff22");
    private static final UUID FILTER_WRONG_ID_2 = UUID.fromString("71001656-60f1-4686-b57b-6bc7aa77a222");

    private WireMockServer wireMock;

    @Before
    public void specificSetUp() throws IOException {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_4, LOAD_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_5, LOAD_ID_5, 20, 200, 2.0, "cn10", 12, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_6, LOAD_ID_6, 11, 120, 4.0, "cn10", 13, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v6"), LOAD_ID_7, LOAD_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_8, LOAD_ID_8, 10, 130, 3.0, "cn10", 15, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_9, LOAD_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v5"), LOAD_ID_10, LOAD_ID_10, 12, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP);
        // to avoid changes on global network, we can set anything before any test
        getNetwork().getLoad("v1load").setQ0(10);
        getNetwork().getLoad("v1load").setP0(180);
        getNetwork().getLoad("v6load").setQ0(10);
        getNetwork().getLoad("v6load").setP0(200);
        wireMock = new WireMockServer(wireMockConfig().dynamicPort());
        wireMock.start();

        var identifiableAttributes1 = IdentifiableAttributes.builder().id(LOAD_ID_4)
                .type(IdentifiableType.LOAD).distributionKey(1.0).build();
        var identifiableAttributes2 = IdentifiableAttributes.builder().id(LOAD_ID_5)
                .type(IdentifiableType.LOAD).distributionKey(2.0).build();
        var identifiableAttributes3 = IdentifiableAttributes.builder().id("Wrongid1")
                .type(IdentifiableType.LOAD).build();
        var identifiableAttributes4 = IdentifiableAttributes.builder().id(LOAD_ID_7)
                .type(IdentifiableType.LOAD).build();
        var identifiableAttributes5 = IdentifiableAttributes.builder().id(LOAD_ID_4)
                .type(IdentifiableType.LOAD).build();
        var identifiableAttributes6 = IdentifiableAttributes.builder().id(LOAD_ID_5)
                .type(IdentifiableType.LOAD).build();
        var filterWithWrongIds = FilterEquipments.builder().filterId(FILTER_WRONG_ID_1)
                .identifiableAttributes(List.of(identifiableAttributes1, identifiableAttributes2))
                .notFoundEquipments(List.of("wrongID"))
                .build();
        var filterWithWrongIds2 = FilterEquipments.builder().filterId(FILTER_WRONG_ID_1)
                .identifiableAttributes(List.of(identifiableAttributes1, identifiableAttributes2))
                .notFoundEquipments(List.of())
                .build();
        var filterWithWrongIds3 = FilterEquipments.builder().filterId(FILTER_WRONG_ID_2)
                .identifiableAttributes(List.of(identifiableAttributes3, identifiableAttributes4))
                .notFoundEquipments(List.of("Wrongid1"))
                .build();
        var filterWithNoDK = FilterEquipments.builder().filterId(FILTER_NO_DK)
                .identifiableAttributes(List.of(identifiableAttributes5, identifiableAttributes6))
                .notFoundEquipments(List.of())
                .build();
        var filterWithWrongIdsJson = mapper.writeValueAsString(List.of(filterWithWrongIds));
        var filterWithWrongIds2Json = mapper.writeValueAsString(List.of(filterWithWrongIds2, filterWithWrongIds3));
        var filterWithNoDKJson = mapper.writeValueAsString(List.of(filterWithNoDK));

        String networkParams = "?networkUuid=" + ((NetworkImpl) getNetwork()).getUuid() + "&variantId=variant_1";
        String params = "&ids=" + String.join(",", FILTER_ID_5.toString(), FILTER_ID_3.toString(), FILTER_ID_4.toString(), FILTER_ID_2.toString(), FILTER_ID_1.toString());
        String path = "/v1/filters/export";
        wireMock.stubFor(WireMock.get(path + networkParams + params)
                .willReturn(WireMock.ok()
                        .withBody(resourceToString())
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_WRONG_ID_1)
                .willReturn(WireMock.ok()
                        .withBody(filterWithWrongIdsJson)
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + String.join(",", FILTER_WRONG_ID_1.toString(), FILTER_WRONG_ID_2.toString()))
                .willReturn(WireMock.ok()
                        .withBody(filterWithWrongIds2Json)
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_NO_DK)
                .willReturn(WireMock.ok()
                        .withBody(filterWithNoDKJson)
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_NOT_FOUND_ID)
                .willReturn(WireMock.notFound()
                        .withHeader("Content-Type", "application/json")));
        FilterService.setFilterServerBaseUri(wireMock.baseUrl());
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {

        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter 1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter 3")
                .build();

        var filter4 = FilterInfos.builder()
                .id(FILTER_ID_4)
                .name("filter 4")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 5")
                .build();

        var variation1 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(30D)
                .filters(List.of(filter1, filter2))
                .build();

        var variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(20D)
                .filters(List.of(filter1, filter3))
                .build();

        var variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(50D)
                .filters(List.of(filter1))
                .build();

        var variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(500D)
                .filters(List.of(filter4, filter5))
                .build();

        return LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1, variation2, variation3, variation4))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 5")
                .build();

        var variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(500D)
                .filters(List.of(filter5))
                .build();

        return LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation5))
                .build();
    }

    @Test
    public void testFilterWithWrongIds() throws Exception {
        var filter = FilterInfos.builder()
                .name("filter")
                .id(FILTER_WRONG_ID_1)
                .build();
        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();
        var generatorScalingInfo = LoadScalingInfos.builder()
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(generatorScalingInfo);

        var response = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError())
                .andReturn();

        var errorMsg = "All filters contains equipments with wrong ids";

        assertEquals(new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, errorMsg).getMessage(), response.getResponse().getContentAsString());
    }

    @Test
    public void testWithWrongFilter() throws Exception {
        var filter = FilterInfos.builder()
                .id(FILTER_NOT_FOUND_ID)
                .name("filter 1")
                .build();

        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(100D)
                .filters(List.of(filter))
                .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        var result = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn().getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(NetworkModificationException.Type.FILTERS_NOT_FOUND, "404 NOT_FOUND").getMessage(), result);
    }

    @Test
    public void testVentilationModeWithoutDistributionKey() throws Exception {
        var filter = FilterInfos.builder()
                .id(FILTER_NO_DK)
                .name("filter")
                .build();

        var variation1 = ScalingVariationInfos.builder()
                .variationValue(100D)
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .filters(List.of(filter))
                .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        var result = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn().getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "This mode is available only for equipment with distribution key").getMessage(), result);
    }

    @Test
    public void testScalingCreationWithWarning() throws Exception {
        var filter = FilterInfos.builder()
                .name("filter1")
                .id(FILTER_WRONG_ID_1)
                .build();
        var filter2 = FilterInfos.builder()
                .name("filter2")
                .id(FILTER_WRONG_ID_2)
                .build();
        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(100D)
                .filters(List.of(filter, filter2))
                .build();
        var loadScalingInfo = LoadScalingInfos.builder()
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(loadScalingInfo);

        var response = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        assertNotNull(response.getResponse().getContentAsString());
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadScalingInfos.createMatcherLoadScalingInfos((LoadScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(150.39, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(166.66, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(195.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(95.58, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(152.84, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(115.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(191.17, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(213.33, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
        assertEquals(283.33, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(266.66, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(180.0, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_5).getP0(), 0);
        assertEquals(120.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_7).getP0(), 0);
        assertEquals(130.0, getNetwork().getLoad(LOAD_ID_8).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_9).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_10).getP0(), 0);
    }

    private String resourceToString() throws IOException {
        return new String(ByteStreams.toByteArray(Objects.requireNonNull(getClass().getResourceAsStream("/filter_equipments.json"))), StandardCharsets.UTF_8);
    }

    @After
    public void shutDown() {
        wireMock.shutdown();
    }
}
