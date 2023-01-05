/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.google.common.io.ByteStreams;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationApplication;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.GeneratorScalingInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.MatcherGeneratorScalingInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.cloud.stream.binder.test.TestChannelBinderConfiguration;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.UUID;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.wireMockConfig;
import static org.gridsuite.modification.server.utils.NetworkUtil.createGenerator;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringRunner.class)
@AutoConfigureMockMvc
@SpringBootTest
@ContextConfiguration(classes = {NetworkModificationApplication.class, TestChannelBinderConfiguration.class})
public class GeneratorScalingModificationTest extends AbstractNetworkModificationTest {

    private static final UUID GENERATOR_SCALING_ID = UUID.randomUUID();
    private static final String FILTER_ID_1 = "bdefd63f-6cd8-4686-b57b-6bc7aaffa202";
    private static final String FILTER_ID_2 = "bdfad63f-6fe6-4686-b57b-6bc7aa11a202";
    private static final String FILTER_ID_3 = "00bd063f-611f-4686-b57b-6bc7aa00a202";
    private static final String FILTER_ID_4 = "6f11d63f-6f06-4686-b57b-6bc7aa66a202";
    private static final String FILTER_ID_5 = "7100163f-60f1-4686-b57b-6bc7aa77a202";
    private static final String FILTER_WRONG_ID_1 = UUID.randomUUID().toString();
    private static final String FILTER_WRONG_ID_2 = UUID.randomUUID().toString();
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

    private WireMockServer wireMock;

    @Autowired
    private FilterService filterService;

    @Before
    public void specificSetUp() throws IOException {
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_4, 3, 100, 1.0, "cn10", 11, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v1"), GENERATOR_ID_5, 20, 200, 1.0, "cn10", 12, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v2"), GENERATOR_ID_6, 11, 100, 1.0, "cn10", 13, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v6"), GENERATOR_ID_7, 10, 200, 1.0, "cn10", 14, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v3"), GENERATOR_ID_8, 10, 100, 1.0, "cn10", 15, ConnectablePosition.Direction.TOP, 500, -1);
        createGenerator(getNetwork().getVoltageLevel("v4"), GENERATOR_ID_9, 10, 200, 1.0, "cn10", 16, ConnectablePosition.Direction.TOP, 2000, -1);
        createGenerator(getNetwork().getVoltageLevel("v5"), GENERATOR_ID_10, 10, 100, 1.0, "cn10", 17, ConnectablePosition.Direction.TOP, 500, -1);

        wireMock = new WireMockServer(wireMockConfig().dynamicPort());
        wireMock.start();

        var filterWithWrongIds = "[{\"filterId\":\"" + FILTER_WRONG_ID_1 + "\",\"identifiableAttributes\":[{\"id\":\"wrongId1\",\"type\":\"GENERATOR\",\"distributionKey\":1},{\"id\":\"wrongId2\",\"type\":\"GENERATOR\",\"distributionKey\":2}],\"notFoundEquipments\":[\"wrongId1\",\"wrongId2\"]}]";
        var filterWithWrongIds2 = "[{\"filterId\":\"bdefd63f-6cd8-4686-b57b-6bc7aaffa202\",\"identifiableAttributes\":[{\"id\":\"idGenerator\",\"type\":\"GENERATOR\",\"distributionKey\":1},{\"id\":\"gen5\",\"type\":\"GENERATOR\",\"distributionKey\":2}],\"notFoundEquipments\":[]},{\"filterId\":\"bdfad63f-6fe6-4686-b57b-6bc7aa11a202\",\"identifiableAttributes\":[{\"id\":\"Wrongid1\",\"type\":\"GENERATOR\"},{\"id\":\"gen7\",\"type\":\"GENERATOR\"}],\"notFoundEquipments\":[\"Wrongid1\"]}]";
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

        filterService.setFilterServerBaseUri(wireMock.baseUrl());
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
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(generatorScalingInfo);

        var response = mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError())
                .andReturn();

        assertEquals(response.getResponse().getContentAsString(), "GENERATOR_SCALING_ERROR : All filters contains equipments with wrong ids");
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
        var generatorScalingInfo = GeneratorScalingInfos.builder()
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(false)
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
                .name("filter 3")
                .build();

        var filter5 = FilterInfos.builder()
                .id(FILTER_ID_5)
                .name("filter 3")
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
                .date(ZonedDateTime.now())
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(true)
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
                .date(ZonedDateTime.now())
                .type(ModificationType.GENERATOR_SCALING)
                .isIterative(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation5))
                .build();
    }

    @Override
    protected MatcherGeneratorScalingInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherGeneratorScalingInfos.createMatcherGeneratorScalingInfos((GeneratorScalingInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 68.82, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 75.43, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 42.1, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 125.0, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 273.27, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 150, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 225, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 116.66, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 233.33, 0.01D);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 116.66, 0.01D);
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_1).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_2).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_3).getTargetP(), 42.1, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_4).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_5).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_6).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_7).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_8).getTargetP(), 100, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_9).getTargetP(), 200, 0);
        assertEquals(getNetwork().getGenerator(GENERATOR_ID_10).getTargetP(), 100, 0);
    }

    private String resourceToString() throws IOException {
        return new String(ByteStreams.toByteArray(Objects.requireNonNull(getClass().getResourceAsStream("/Filter_equipments.json"))), StandardCharsets.UTF_8);
    }

    @After
    public void shutDown() {
        wireMock.shutdown();
    }
}
