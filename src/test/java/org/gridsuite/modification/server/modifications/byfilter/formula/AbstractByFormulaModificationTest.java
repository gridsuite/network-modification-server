/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.formula;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.dto.byfilter.formula.Operator;
import org.gridsuite.modification.dto.byfilter.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */
@Tag("IntegrationTest")
abstract class AbstractByFormulaModificationTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final UUID FILTER_ID_3 = UUID.randomUUID();
    protected static final UUID FILTER_ID_4 = UUID.randomUUID();
    protected static final UUID FILTER_ID_5 = UUID.randomUUID();
    protected static final UUID FILTER_ID_6 = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ALL_WRONG_IDS = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ONE_WRONG_ID = UUID.randomUUID();
    protected final FilterInfos filter1 = new FilterInfos(FILTER_ID_1, "filter1");
    protected final FilterInfos filter2 = new FilterInfos(FILTER_ID_2, "filter2");
    protected final FilterInfos filter3 = new FilterInfos(FILTER_ID_3, "filter3");
    protected final FilterInfos filter4 = new FilterInfos(FILTER_ID_4, "filter4");
    protected final FilterInfos filter5 = new FilterInfos(FILTER_ID_5, "filter5");
    protected final FilterInfos filter6 = new FilterInfos(FILTER_ID_6, "filter6");
    protected final FilterInfos filterWithOneWrongId = new FilterInfos(FILTER_WITH_ONE_WRONG_ID, "filterWithOneWrongId");

    protected static final String PATH = "/v1/filters/metadata";

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(getIdentifiableType()));
    }

    @BeforeEach
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    @Test
    public void testByModificationError() throws Exception {

        // Test with empty list of formulas
        checkCreationApplicationStatus(ByFormulaModificationInfos.builder().identifiableType(getIdentifiableType()).formulaInfosList(List.of()).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with empty list of filters in formula
        List<FormulaInfos> formulaInfosWithNoFilters = getFormulaInfos().stream().peek(formula -> formula.setFilters(List.of())).toList();
        checkCreationApplicationStatus(ByFormulaModificationInfos.builder().identifiableType(getIdentifiableType()).formulaInfosList(formulaInfosWithNoFilters).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with editedField = null
        FormulaInfos formulaInfosWithNoEditedField = FormulaInfos.builder()
                .fieldOrValue1(ReferenceFieldOrValue.builder().value(50.).build())
                .fieldOrValue2(ReferenceFieldOrValue.builder().value(50.).build())
                .operator(Operator.ADDITION)
                .filters(List.of())
                .build();
        checkCreationApplicationStatus(ByFormulaModificationInfos.builder().identifiableType(getIdentifiableType()).formulaInfosList(List.of(formulaInfosWithNoEditedField)).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);
    }

    protected void checkCreateWithWarning(List<FormulaInfos> formulaInfos, List<IdentifierListFilterEquipmentAttributes> existingEquipmentList) throws Exception {
        AbstractFilter filter = getFilterEquipments(FILTER_WITH_ONE_WRONG_ID, existingEquipmentList);

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + FILTER_WITH_ONE_WRONG_ID))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(List.of(FILTER_WITH_ONE_WRONG_ID)), false);
    }

    protected void checkCreateWithError(List<FormulaInfos> formulaInfos, List<AbstractFilter> filterEquipments) throws Exception {
        String filterIds = filterEquipments.stream()
                .map(AbstractFilter::getId)
                .map(UUID::toString)
                .collect(Collectors.joining(","));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + filterIds))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filterEquipments))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId,
                PATH,
                handleQueryParams(filterEquipments.stream().map(AbstractFilter::getId).collect(Collectors.toList())),
                false);
    }

    @Test
    public void testModificationWithAllWrongEquipmentIds() throws Exception {
        AbstractFilter filter = getFilterEquipments(FILTER_WITH_ALL_WRONG_IDS, List.of());

        List<FormulaInfos> formulaInfos = getFormulaInfos().stream()
                .peek(formula -> formula.setFilters(List.of(new FilterInfos(FILTER_WITH_ALL_WRONG_IDS, "filterWithWrongId"))))
                .toList();

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + FILTER_WITH_ALL_WRONG_IDS))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(List.of(FILTER_WITH_ALL_WRONG_IDS)), false);
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + ".{2,}"))
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
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + ".{2,}"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    protected void checkCreationApplicationStatus(ByFormulaModificationInfos byFormulaModificationInfos,
                                                  NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        String modificationToCreateJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(byFormulaModificationInfos, List.of(buildApplicationContext())));

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        Optional<NetworkModificationsResult> networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());
        assertEquals(1, extractApplicationStatus(networkModificationsResult.get()).size());
        assertEquals(applicationStatus, extractApplicationStatus(networkModificationsResult.get()).getFirst());
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ByFormulaModificationInfos buildModification() {
        return ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(getFormulaInfos())
                .stashed(false)
                .build();
    }

    @Override
    protected ByFormulaModificationInfos buildModificationUpdate() {
        return ByFormulaModificationInfos.builder()
                .identifiableType(getIdentifiableType())
                .formulaInfosList(getUpdatedFormulaInfos())
                .stashed(false)
                .build();
    }

    protected IdentifierListFilterEquipmentAttributes getIdentifiableAttributes(String id, Double distributionKey) {
        return new IdentifierListFilterEquipmentAttributes(id, distributionKey);
    }

    protected AbstractFilter getFilterEquipments(UUID filterID, List<IdentifierListFilterEquipmentAttributes> identifiableAttributes) {
        return IdentifierListFilter.builder().id(filterID).modificationDate(new Date()).equipmentType(getEquipmentType())
            .filterEquipmentsAttributes(identifiableAttributes)
            .build();
    }

    protected FormulaInfos getFormulaInfo(String editedField,
                                List<FilterInfos> filters,
                                Operator operator,
                                ReferenceFieldOrValue fieldOrValue1,
                                ReferenceFieldOrValue fieldOrValue2) {
        return FormulaInfos.builder()
                .editedField(editedField)
                .filters(filters)
                .operator(operator)
                .fieldOrValue1(fieldOrValue1)
                .fieldOrValue2(fieldOrValue2)
                .build();
    }

    protected Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    protected String getPath(boolean isRegexPath) {
        if (isRegexPath) {
            return "/v1/filters/metadata\\?ids=";
        }
        return "/v1/filters/metadata?ids=";
    }

    protected abstract void createEquipments();

    protected abstract List<AbstractFilter> getTestFilters();

    protected abstract List<FormulaInfos> getFormulaInfos();

    protected abstract List<FormulaInfos> getUpdatedFormulaInfos();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();
}
