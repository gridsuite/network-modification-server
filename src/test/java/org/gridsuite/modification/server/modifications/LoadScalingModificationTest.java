package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterInfos;
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
import org.springframework.beans.factory.annotation.Autowired;
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
    private static final String FILTER_WRONG_ID = UUID.randomUUID().toString();
    private static final String FILTER_NOT_FOUND_ID = UUID.randomUUID().toString();
    private static final String FILTER_NO_DK = UUID.randomUUID().toString();
    private static final String FILTER_WRONG_ID_1 = UUID.randomUUID().toString();
    private static final String FILTER_WRONG_ID_2 = UUID.randomUUID().toString();

    private WireMockServer wireMock;

    @Autowired
    private FilterService filterService;

    @Before
    public void specificSetUp() throws IOException {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_4, LOAD_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v1"), LOAD_ID_5, LOAD_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v2"), LOAD_ID_6, LOAD_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v6"), LOAD_ID_7, LOAD_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v3"), LOAD_ID_8, LOAD_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v4"), LOAD_ID_9, LOAD_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP);
        createLoad(getNetwork().getVoltageLevel("v5"), LOAD_ID_10, LOAD_ID_10, 12, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP);
        // to avoid changes on global network, we can set anything before any test
        getNetwork().getLoad("v1load").setQ0(10);
        getNetwork().getLoad("v1load").setP0(100);
        getNetwork().getLoad("v6load").setQ0(10);
        getNetwork().getLoad("v6load").setP0(100);
        wireMock = new WireMockServer(wireMockConfig().dynamicPort());
        wireMock.start();

        var filterWithWrongIds = "[{\"filterId\":\"" + FILTER_WRONG_ID_1 + "\",\"identifiableAttributes\":[{\"id\":\"idLoad\",\"type\":\"LOAD\",\"distributionKey\":1},{\"id\":\"load5\",\"type\":\"LOAD\",\"distributionKey\":2}],\"notFoundEquipments\":[\"wrongID\"]}]";
        var filterWithWrongIds2 = "[{\"filterId\":\"bdefd63f-6cd8-4686-b57b-6bc7aaffa202\",\"identifiableAttributes\":[{\"id\":\"idLoad\",\"type\":\"LOAD\",\"distributionKey\":1},{\"id\":\"load5\",\"type\":\"LOAD\",\"distributionKey\":2}],\"notFoundEquipments\":[]},{\"filterId\":\"bdfad63f-6fe6-4686-b57b-6bc7aa11a202\",\"identifiableAttributes\":[{\"id\":\"Wrongid1\",\"type\":\"LOAD\"},{\"id\":\"load7\",\"type\":\"LOAD\"}],\"notFoundEquipments\":[\"Wrongid1\"]}]";
        var filterWithNoDK = "[{\"filterId\":\"" + FILTER_NO_DK + "\",\"identifiableAttributes\":[{\"id\":\"idLoad\",\"type\":\"LOAD\"},{\"id\":\"load5\",\"type\":\"LOAD\"}],\"notFoundEquipments\":[]}]";

        String networkParams = "?networkUuid=" + ((NetworkImpl) getNetwork()).getUuid() + "&variantId=variant_1";
        String params = "&ids=" + String.join(",", List.of(FILTER_ID_1, FILTER_ID_2, FILTER_ID_3, FILTER_ID_4, FILTER_ID_5));
        String path = "/v1/filters/export";
        wireMock.stubFor(WireMock.get(path + networkParams + params)
                .willReturn(WireMock.ok()
                        .withBody(resourceToString())
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_WRONG_ID_1)
                .willReturn(WireMock.ok()
                        .withBody(filterWithWrongIds)
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_WRONG_ID_2)
                .willReturn(WireMock.ok()
                        .withBody(filterWithWrongIds2)
                        .withHeader("Content-Type", "application/json")));

        wireMock.stubFor(WireMock.get(path + networkParams + "&ids=" + FILTER_NO_DK)
                .willReturn(WireMock.ok()
                        .withBody(filterWithNoDK)
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
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(100D)
                .filters(List.of(filter1, filter2))
                .build();

        var variation2 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.REGULAR_DISTRIBUTION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(50D)
                .filters(List.of(filter1, filter3))
                .build();

        var variation3 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.TAN_FIXED)
                .variationValue(150D)
                .filters(List.of(filter1))
                .build();

        var variation4 = ScalingVariationInfos.builder()
                .variationMode(VariationMode.VENTILATION)
                .reactiveVariationMode(ReactiveVariationMode.CONSTANT_Q)
                .variationValue(400D)
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
                .variationValue(50D)
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

        assertEquals("LOAD_SCALING_ERROR : All filters contains equipments with wrong ids", response.getResponse().getContentAsString());
    }

    @Test
    public void testWithWrongFilter() throws Exception {
        var filter = FilterInfos.builder()
                .id(FILTER_NOT_FOUND_ID)
                .name("filter 1")
                .build();

        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
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
                .andExpect(status().is5xxServerError()).andReturn().getResponse().getContentAsString();
        assertEquals(new NetworkModificationException(NetworkModificationException.Type.LOAD_SCALING_ERROR, "404 Not Found: [no body]").getMessage(), result);
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
                .name("filter")
                .id(FILTER_WRONG_ID_2)
                .build();
        var variation = ScalingVariationInfos.builder()
                .variationMode(VariationMode.PROPORTIONAL)
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
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_1).getP0(), 0.01D);
        assertEquals(266.6666564941406, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(75.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
        assertEquals(66.66666793823242, getNetwork().getLoad(LOAD_ID_4).getP0(), 0.01D);
        assertEquals(8.33333969116211, getNetwork().getLoad(LOAD_ID_5).getP0(), 0.01D);
        assertEquals(75.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0.01D);
        assertEquals(133.33333587646484, getNetwork().getLoad(LOAD_ID_7).getP0(), 0.01D);
        assertEquals(233.3333282470703, getNetwork().getLoad(LOAD_ID_8).getP0(), 0.01D);
        assertEquals(333.3333282470703, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(366.6666564941406, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_1).getP0(), 0);
        assertEquals(0.0, getNetwork().getLoad(LOAD_ID_2).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_3).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_4).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_5).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_6).getP0(), 0);
        assertEquals(200.0, getNetwork().getLoad(LOAD_ID_7).getP0(), 0);
        assertEquals(100.0, getNetwork().getLoad(LOAD_ID_8).getP0(), 0);
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
