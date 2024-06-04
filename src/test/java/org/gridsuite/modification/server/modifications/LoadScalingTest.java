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
import org.gridsuite.modification.server.ReactiveVariationMode;
import org.gridsuite.modification.server.VariationMode;
import org.gridsuite.modification.server.VariationType;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.hamcrest.core.IsNull;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
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
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Tag("IntegrationTest")
public class LoadScalingTest extends AbstractNetworkModificationTest {
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

    public static final String LOAD_WRONG_ID_1 = "wrongId1";

    public static final String PATH = "/v1/filters/metadata";

    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        createLoads();
    }

    private void createLoads() {
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

    private List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_1, 1.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_2, 2.0)))
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_3, 2.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_4, 5.0)))
            .build();
        IdentifierListFilter filter3 = IdentifierListFilter.builder().id(FILTER_ID_3).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_5, 6.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_6, 7.0)))
            .build();
        IdentifierListFilter filter4 = IdentifierListFilter.builder().id(FILTER_ID_4).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_7, 3.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_8, 8.0)))
            .build();
        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_9, 0.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_10, 9.0)))
            .build();

        return List.of(filter1, filter2, filter3, filter4, filter5);
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.LOAD));
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
        IdentifierListFilter noDistributionKeyFilter = IdentifierListFilter.builder().id(FILTER_NO_DK).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_2, null),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_3, null)))
            .build();

        UUID stubNonDistributionKey = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_NO_DK)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(noDistributionKeyFilter)))
                        .withHeader("Content-Type", "application/json"))).getId();
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
            .date(OffsetDateTime.now().truncatedTo(ChronoUnit.MICROS))
            .variationType(VariationType.DELTA_P)
            .variations(List.of(variation1))
            .build();

        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(modificationToCreate)).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        wireMockUtils.verifyGetRequest(stubNonDistributionKey, PATH, handleQueryParams(FILTER_NO_DK), false);

        assertEquals(200, getNetwork().getLoad(LOAD_ID_2).getP0(), 0.01D);
        assertEquals(200, getNetwork().getLoad(LOAD_ID_3).getP0(), 0.01D);
    }

    @Test
    public void testFilterWithWrongIds() throws Exception {
        IdentifierListFilter wrongIdFilter1 = IdentifierListFilter.builder().id(FILTER_WRONG_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of())
            .build();

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
        UUID stubWithWrongId = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_WRONG_ID_1)
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(wrongIdFilter1)))
                        .withHeader("Content-Type", "application/json"))).getId();

        mockMvc.perform(post(getNetworkModificationUri())
                .content(mapper.writeValueAsString(loadScalingInfo))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(loadScalingInfo.getErrorType().name() + ": There is no valid equipment ID among the provided filter(s)",
                "invalidFilters", reportService);
        wireMockUtils.verifyGetRequest(stubWithWrongId, PATH, handleQueryParams(FILTER_WRONG_ID_1), false);
    }

    @Test
    public void testScalingCreationWithWarning() throws Exception {
        String params = "(" + FILTER_ID_5 + "|" + FILTER_WRONG_ID_2 + ")";

        IdentifierListFilter filter5 = IdentifierListFilter.builder().id(FILTER_ID_5).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_ID_9, 0.0),
                new IdentifierListFilterEquipmentAttributes(LOAD_ID_10, 9.0)))
            .build();

        IdentifierListFilter wrongIdFilter2 = IdentifierListFilter.builder().id(FILTER_WRONG_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes(LOAD_WRONG_ID_1, 2.0)))
            .build();

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

        UUID stubMultipleWrongIds = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + params + "," + params))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(wrongIdFilter2, filter5)))
                        .withHeader("Content-Type", "application/json"))).getId();

        mockMvc.perform(post(getNetworkModificationUri())
                .content(mapper.writeValueAsString(loadScalingInfo))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isOk(),
                content().string(IsNull.notNullValue())
            );

        wireMockUtils.verifyGetRequest(stubMultipleWrongIds, PATH, Map.of("ids", WireMock.matching(".*")), false);
        assertEquals(600, getNetwork().getLoad(LOAD_ID_9).getP0(), 0.01D);
        assertEquals(300, getNetwork().getLoad(LOAD_ID_10).getP0(), 0.01D);
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
            .date(OffsetDateTime.now().truncatedTo(ChronoUnit.MICROS))
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
            .date(OffsetDateTime.now().truncatedTo(ChronoUnit.MICROS))
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
    public void testProportionalAllConnected() {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of());
    }

    @Test
    public void testProportionalAndVentilationLD1Disconnected() {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1"));
        testVariationWithSomeDisconnections(VariationMode.VENTILATION, List.of("LD1"));
    }

    @Test
    public void testProportionalOnlyLD6Connected() {
        testVariationWithSomeDisconnections(VariationMode.PROPORTIONAL, List.of("LD1", "LD2", "LD3", "LD4", "LD5"));
    }

    @SneakyThrows
    private void testVariationWithSomeDisconnections(VariationMode variationMode, List<String> loadsToDisconnect) {
        // use a dedicated network where we can easily disconnect loads
        setNetwork(Network.read(Paths.get(Objects.requireNonNull(this.getClass().getClassLoader().getResource("fourSubstations_testsOpenReac.xiidm")).toURI())));

        // disconnect some loads (must not be taken into account by the variation modification)
        loadsToDisconnect.forEach(l -> getNetwork().getLoad(l).getTerminal().disconnect());
        List<String> modifiedLoads = Stream.of("LD1", "LD2", "LD3", "LD4", "LD5", "LD6")
                .filter(l -> !loadsToDisconnect.contains(l))
                .toList();

        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_ALL_LOADS).modificationDate(new Date()).equipmentType(EquipmentType.LOAD)
            .filterEquipmentsAttributes(List.of(new IdentifierListFilterEquipmentAttributes("LD1", 0.0),
                    new IdentifierListFilterEquipmentAttributes("LD2", 100.0),
                    new IdentifierListFilterEquipmentAttributes("LD3", 100.0),
                    new IdentifierListFilterEquipmentAttributes("LD4", 100.0),
                    new IdentifierListFilterEquipmentAttributes("LD5", 100.0),
                    new IdentifierListFilterEquipmentAttributes("LD6", 100.0)))
            .build();

        UUID subFilter = wireMockServer.stubFor(WireMock.get(getPath(false) + FILTER_ID_ALL_LOADS)
            .willReturn(WireMock.ok()
                .withBody(mapper.writeValueAsString(List.of(filter1)))
                .withHeader("Content-Type", "application/json"))).getId();

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
                .date(OffsetDateTime.now(ZoneOffset.UTC).truncatedTo(ChronoUnit.MICROS))
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation))
                .build();

        String modificationToCreateJson = mapper.writeValueAsString(loadScalingInfo);
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(modificationToCreateJson)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // If we sum the P0 for all expected modified loads, we should have the requested variation value
        double connectedLoadsConstantP = modifiedLoads
                .stream()
                .map(g -> getNetwork().getLoad(g).getP0())
                .reduce(0D, Double::sum);
        assertEquals(variationValue, connectedLoadsConstantP, 0.001D);

        wireMockUtils.verifyGetRequest(subFilter, PATH, Map.of("ids", WireMock.matching(".*")), false);
    }
}
