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
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.service.FilterService.setFilterServerBaseUri;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class LoadScalingModificationTest extends AbstractNetworkModificationTest {

    private static final UUID LOAD_SCALING_ID = UUID.randomUUID();
    private static final UUID FILTER_ID_1 = UUID.fromString("bdefd63f-6cd8-4686-b57b-6bc7aaffa202");
    private static final UUID FILTER_ID_2 = UUID.fromString("bdfad63f-6fe6-4686-b57b-6bc7aa11a202");
    private static final UUID FILTER_ID_3 = UUID.fromString("00bd063f-611f-4686-b57b-6bc7aa00a202");
    private static final UUID FILTER_ID_4 = UUID.fromString("6f11d63f-6f06-4686-b57b-6bc7aa66a202");
    private static final UUID FILTER_ID_5 = UUID.fromString("7100163f-60f1-4686-b57b-6bc7aa77a202");
    private static final UUID FILTER_NOT_FOUND_ID = UUID.randomUUID();
    private static final UUID FILTER_NO_DK = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_WRONG_ID_2 = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
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

    @Autowired
    private FilterService filterService;

    @Before
    public void specificSetUp() throws IOException {
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

        String networkParams = "?networkUuid=" + ((NetworkImpl) getNetwork()).getUuid() + "&variantId=variant_1";
        String params = "&ids=" + Stream.of(FILTER_ID_5, FILTER_ID_3, FILTER_ID_4, FILTER_ID_2, FILTER_ID_1).map(UUID::toString).collect(Collectors.joining(","));
        String path = "/v1/filters/export";

        wireMock.stubFor(WireMock.get(path + networkParams + params)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter1, filter2, filter3, filter4, filter5)))
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_WRONG_ID_1)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(wrongIdFilter1)))
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_ID_5 + "," + FILTER_WRONG_ID_2)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(wrongIdFilter2, filter5)))
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_NO_DK)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(noDistributionKeyFilter)))
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_NOT_FOUND_ID)
                .willReturn(WireMock.notFound()
                        .withHeader("Content-Type", "application/json")));

        setFilterServerBaseUri(wireMock.baseUrl());
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

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
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
        var loadScalingInfo = LoadScalingInfos.builder()
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(loadScalingInfo);

        var response = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError())
                .andReturn();

        assertEquals(new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "All filters contains equipments with wrong ids").getMessage(),
                response.getResponse().getContentAsString());
    }

    @Test
    public void testScalingCreationWithWarning() throws Exception {
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
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(900D)
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
        assertEquals(-199.99, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(-99.99, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
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
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(50D)
                .filters(List.of(filter2))
                .build();

        var variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(50D)
                .filters(List.of(filter4))
                .build();

        var variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(50D)
                .filters(List.of(filter1, filter5))
                .build();

        var variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(100D)
                .filters(List.of(filter3))
                .build();

        var variation5 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(50D)
                .filters(List.of(filter3))
                .build();

        return LoadScalingInfos.builder()
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
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
                .reactiveVariationMode(ReactiveVariationMode.TAN_PHI_FIXED)
                .variationValue(50D)
                .filters(List.of(filter5))
                .build();

        return LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation5))
                .build();
    }

    @Override
    protected MatcherLoadScalingInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadScalingInfos.createMatcherLoadScalingInfos((LoadScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(108.33, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(216.66, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(175.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(75.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(112.5, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(57.5, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(186.36, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(93.63, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
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

    @After
    public void shutDown() {
        wireMock.shutdown();
    }
}
