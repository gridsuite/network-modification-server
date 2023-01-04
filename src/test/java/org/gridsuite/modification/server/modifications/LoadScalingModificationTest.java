package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.LoadScalingInfos;
import org.gridsuite.modification.server.dto.LoadScalingVariation;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.MatcherLoadScalingInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LOAD_SCALING_ERROR;
import static org.gridsuite.modification.server.utils.NetworkUtil.createLoad;
import static org.junit.Assert.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LoadScalingModificationTest extends AbstractNetworkModificationTest {

    private static final String FILTER_ID_1 = "bdefd63f-6cd8-4686-b57b-6bc7aaffa202";
    private static final String FILTER_ID_2 = "bdfad63f-6fe6-4686-b57b-6bc7aa11a202";
    private static final String FILTER_ID_3 = "00bd063f-611f-4686-b57b-6bc7aa00a202";
    private static final String FILTER_ID_4 = "6f11d63f-6f06-4686-b57b-6bc7aa66a202";
    private static final String FILTER_ID_5 = "7100163f-60f1-4686-b57b-6bc7aa77a202";

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

    private WireMockServer wireMock;

    @Autowired
    private FilterService filterService;

    @Before
    public void specificSetUp() throws IOException {
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_4, LOAD_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_5, LOAD_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_6, LOAD_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v6"), LOAD_ID_7, LOAD_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_8, LOAD_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_9, LOAD_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v5"), LOAD_ID_10, LOAD_ID_10, 12, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP);
        // to avoid changes on global network, we can set anything before any test
        getNetwork().getLoad("v5load").setQ0(10);
        getNetwork().getLoad("v5load").setP0(100);
        wireMock = new WireMockServer(wireMockConfig().dynamicPort());
        wireMock.start();
        wireMock.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata.*"))
                .willReturn(WireMock.ok()
                        .withBody(resourceToString("/filter_equipments.json"))
                        .withHeader("Content-Type", "application/json")));
        filterService.setFilterServerBaseUri(wireMock.baseUrl());
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

        var variation1 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(100D)
                .filters(List.of(filter1, filter2))
                .build();

        var variation2 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(50D)
                .filters(List.of(filter1, filter3))
                .build();

        var variation3 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(150D)
                .filters(List.of(filter1))
                .build();

        var variation4 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(400D)
                .filters(List.of(filter4, filter5))
                .build();

        return LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .loadScalingVariations(List.of(variation1, variation2, variation3, variation4))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 5")
                .build();

        var variation5 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(50D)
                .filters(List.of(filter5))
                .build();

        return LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .loadScalingVariations(List.of(variation5))
                .build();
    }

    @Test
    public void errorWhenScalingWithInvalidFiltersTest() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter 1")
                .build();

        var filter3 = FilterInfos.builder()
                .id(FILTER_ID_3)
                .name("filter 3")
                .build();

        var variation1 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.PROPORTIONAL)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(100D)
                .filters(List.of())
                .build();

        var variation2 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(50D)
                .filters(List.of(filter1, filter3))
                .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .loadScalingVariations(List.of(variation1, variation2))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        var result = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn().getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(LOAD_SCALING_ERROR, "One of the variations does not have a correct filters").getMessage(), result);
    }

    @Test
    public void errorWhenScalingInVentilationModeWithoutDistributionIdsTest() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter 1")
                .build();

        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2")
                .build();

        var variation1 = LoadScalingVariation.builder()
                .activeVariationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(100D)
                .filters(List.of(filter1, filter2))
                .build();

        ModificationInfos modificationToCreate = LoadScalingInfos.builder()
                .uuid(LOAD_SCALING_ID)
                .date(ZonedDateTime.now())
                .type(ModificationType.LOAD_SCALING)
                .variationType(VariationType.DELTA_P)
                .loadScalingVariations(List.of(variation1))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(modificationToCreate);

        var result = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn().getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(LOAD_SCALING_ERROR, "This mode is available only for equipment with distribution key").getMessage(), result);
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadScalingInfos.createMatcherLoadScalingInfos((LoadScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(74.99, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(25.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(133.33, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(424.99, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(125.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(266.66, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
        assertEquals(66.66, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_4).getP0(), 0);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_5).getP0(), 0);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_6).getP0(), 0);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_7).getP0(), 0);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_8).getP0(), 0);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_9).getP0(), 0);
        assertEquals(100, getNetwork().getLoad(LOAD_ID_10).getP0(), 0);
    }

    private String resourceToString(String resource) throws IOException {
        return new String(ByteStreams.toByteArray(Objects.requireNonNull(getClass().getResourceAsStream(resource))), StandardCharsets.UTF_8);
    }

    @After
    public void shutDown() {
        wireMock.shutdown();
    }
}
