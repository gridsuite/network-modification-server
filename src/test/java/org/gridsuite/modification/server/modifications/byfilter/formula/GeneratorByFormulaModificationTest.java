/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.GeneratorField;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
class GeneratorByFormulaModificationTest extends AbstractNetworkModificationTest {
    private static final UUID FILTER_ID_1 = UUID.randomUUID();
    private static final UUID FILTER_ID_2 = UUID.randomUUID();
    private static final String PATH = "/v1/filters/metadata";
    private static final String[] GENERATOR_IDS = {"gen1", "gen2", "gen3", "gen4", "gen5", "gen6", "gen7", "gen8", "gen9", "gen10"};

    @BeforeEach
    void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createGeneratorsNetwork(networkUuid, new NetworkFactoryImpl());
    }

    private static List<AbstractFilter> getTestFilters() {
        IdentifierListFilter filter1 = IdentifierListFilter.builder().id(FILTER_ID_1).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(GENERATOR_IDS[0], GENERATOR_IDS[1], GENERATOR_IDS[2], GENERATOR_IDS[3], GENERATOR_IDS[4])
                .stream().map(id -> new IdentifierListFilterEquipmentAttributes(id, null)).toList())
            .build();
        IdentifierListFilter filter2 = IdentifierListFilter.builder().id(FILTER_ID_2).modificationDate(new Date()).equipmentType(EquipmentType.GENERATOR)
            .filterEquipmentsAttributes(List.of(GENERATOR_IDS[5], GENERATOR_IDS[6], GENERATOR_IDS[7], GENERATOR_IDS[8], GENERATOR_IDS[9])
                .stream().map(id -> new IdentifierListFilterEquipmentAttributes(id, null)).toList())
            .build();
        return List.of(filter1, filter2);
    }

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(IdentifiableType.GENERATOR));
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath() + ".{2,}"))
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
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath() + ".{2,}"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    @Override
    protected ModificationInfos buildModification() {
        FormulaInfos formulaInfos = FormulaInfos.builder()
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .filters(List.of(new FilterInfos(FILTER_ID_1, "filter1"), new FilterInfos(FILTER_ID_2, "filter2")))
                .operator(Operator.ADDITION)
                .fieldOrValue1(ReferenceFieldOrValue.builder().equipmentField(GeneratorField.ACTIVE_POWER_SET_POINT.name()).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(25.).build())
                .build();

        return ByFormulaModificationInfos.builder()
                .stashed(false)
                .identifiableType(IdentifiableType.GENERATOR)
                .formulaInfosList(List.of(formulaInfos))
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        FormulaInfos formulaInfos = FormulaInfos.builder()
                .editedField(GeneratorField.ACTIVE_POWER_SET_POINT.name())
                .filters(List.of(new FilterInfos(FILTER_ID_1, "filter1")))
                .operator(Operator.MULTIPLICATION)
                .fieldOrValue1(ReferenceFieldOrValue.builder().equipmentField(GeneratorField.ACTIVE_POWER_SET_POINT.name()).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(2.).build())
                .build();

        return ByFormulaModificationInfos.builder()
                .stashed(false)
                .identifiableType(IdentifiableType.GENERATOR)
                .formulaInfosList(List.of(formulaInfos))
                .build();
    }

    private static Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private static String getPath() {
        return "/v1/filters/metadata\\?ids=";
    }
}
