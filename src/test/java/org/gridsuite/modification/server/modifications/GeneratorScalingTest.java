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
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.VariationMode;
import org.gridsuite.modification.VariationType;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.GeneratorScalingInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ScalingVariationInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
class GeneratorScalingTest extends AbstractNetworkModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final UUID FILTER_ID_3 = UUID.randomUUID();
    private static final UUID FILTER_ID_4 = UUID.randomUUID();
    private static final UUID FILTER_ID_5 = UUID.randomUUID();
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
    private static final String PATH = "/v1/filters/metadata";

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

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

    private static List<AbstractFilter> getTestFilters() {
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
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath() + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        super.testCreate();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath() + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createGeneratorsNetwork(networkUuid, new NetworkFactoryImpl());
    }

    @Override
    protected ModificationInfos buildModification() {
        var filter1 = FilterInfos.builder().id(FILTER_ID_1).name("filter1").build();
        var filter2 = FilterInfos.builder().id(FILTER_ID_2).name("filter2").build();
        var filter3 = FilterInfos.builder().id(FILTER_ID_3).name("filter3").build();
        var filter4 = FilterInfos.builder().id(FILTER_ID_4).name("filter4").build();
        var filter5 = FilterInfos.builder().id(FILTER_ID_5).name("filter5").build();

        var variation1 = ScalingVariationInfos.builder().variationMode(VariationMode.PROPORTIONAL_TO_PMAX).variationValue(50D).filters(List.of(filter1)).build();
        var variation2 = ScalingVariationInfos.builder().variationMode(VariationMode.REGULAR_DISTRIBUTION).variationValue(50D).filters(List.of(filter2)).build();
        var variation3 = ScalingVariationInfos.builder().variationMode(VariationMode.STACKING_UP).variationValue(50D).filters(List.of(filter3)).build();
        var variation4 = ScalingVariationInfos.builder().variationMode(VariationMode.VENTILATION).variationValue(50D).filters(List.of(filter4)).build();
        var variation5 = ScalingVariationInfos.builder().variationMode(VariationMode.PROPORTIONAL).variationValue(50D).filters(List.of(filter1, filter5)).build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.DELTA_P)
                .variations(List.of(variation1, variation2, variation3, variation4, variation5))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter5 = FilterInfos.builder().id(FILTER_ID_5).name("filter 3").build();
        var variation5 = ScalingVariationInfos.builder().variationMode(VariationMode.PROPORTIONAL).variationValue(50D).filters(List.of(filter5)).build();

        return GeneratorScalingInfos.builder()
                .stashed(false)
                .variationType(VariationType.TARGET_P)
                .variations(List.of(variation5))
                .build();
    }

    private static Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private static String getPath() {
        return "/v1/filters/metadata\\?ids=";
    }
}