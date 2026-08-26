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
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.byfilter.formula.FormulaInfos;
import org.gridsuite.modification.modifications.data.assignment.Operator;
import org.gridsuite.modification.modifications.data.assignment.ReferenceFieldOrValue;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.FilterStub;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import java.util.*;
import java.util.stream.Collectors;
import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
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
    protected final FilterInfos filter1 = new FilterInfos(FILTER_ID_1, "filter1");
    protected final FilterInfos filter2 = new FilterInfos(FILTER_ID_2, "filter2");
    protected final FilterInfos filter3 = new FilterInfos(FILTER_ID_3, "filter3");
    protected final FilterInfos filter4 = new FilterInfos(FILTER_ID_4, "filter4");
    protected final FilterInfos filter5 = new FilterInfos(FILTER_ID_5, "filter5");
    protected final FilterInfos filter6 = new FilterInfos(FILTER_ID_6, "filter6");

    protected static final String PATH = "/v1/standalone-filters";

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(getIdentifiableType()));
    }

    @BeforeEach
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        org.gridsuite.modification.server.service.FilterLoader.setFilterServerBaseUri(wireMockServer.baseUrl());
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

    protected void checkCreateWithError(List<FormulaInfos> formulaInfos, List<FilterStub> filterEquipments) throws Exception {
        checkCreateWithStatus(formulaInfos, filterEquipments, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);
    }

    protected void checkCreateWithStatus(List<FormulaInfos> formulaInfos, List<FilterStub> filterEquipments,
                                         NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        UUID stubId = stubStandaloneFilters(filterEquipments);

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, applicationStatus);

        verifyStandaloneFiltersRequest(stubId, filterEquipments.stream().map(FilterStub::id).toList());
    }

    @Test
    public void testModificationWithAllWrongEquipmentIds() throws Exception {
        FilterStub filter = getFilterEquipments(FILTER_WITH_ALL_WRONG_IDS, Set.of());

        List<FormulaInfos> formulaInfos = getFormulaInfos().stream()
                .peek(formula -> formula.setFilters(List.of(new FilterInfos(FILTER_WITH_ALL_WRONG_IDS, "filterWithWrongId"))))
                .toList();

        UUID stubId = stubStandaloneFilters(List.of(filter));

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        verifyStandaloneFiltersRequest(stubId, List.of(FILTER_WITH_ALL_WRONG_IDS), getFormulaInfos().size());
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(getFormulaInfos().stream()
                .map(formula -> formula.getFilters().stream().map(FilterInfos::getId).toList())
                .toList());

        super.testCreate();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(getFormulaInfos().stream()
                .map(formula -> formula.getFilters().stream().map(FilterInfos::getId).toList())
                .toList());

        super.testCopy();

        verifyStandaloneFiltersRequests(stubs);
    }

    protected void checkCreationApplicationStatus(ByFormulaModificationInfos byFormulaModificationInfos,
                                                  NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        String modificationToCreateJson = getJsonBody(byFormulaModificationInfos, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        MvcResult mvcResult = mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
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

    protected FilterStub getFilterEquipments(UUID filterID, Collection<String> equipmentIds) {
        return new FilterStub(filterID, equipmentFilter(Set.copyOf(equipmentIds)));
    }

    protected Filter equipmentFilter(Set<String> equipmentIds) {
        return IdentifierListFilter.builder()
                .equipmentType(getEquipmentType())
                .equipmentIds(equipmentIds)
                .build();
    }

    private List<StubbedFilterRequest> stubStandaloneFilterRequests(List<List<UUID>> filterIdsList) throws Exception {
        Map<List<UUID>, Integer> requestCounts = new LinkedHashMap<>();
        filterIdsList.forEach(filterIds -> requestCounts.merge(filterIds, 1, Integer::sum));

        Map<UUID, Filter> filtersById = getFilterMapping().entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> equipmentFilter(entry.getValue())));

        List<StubbedFilterRequest> stubbedFilterRequests = new ArrayList<>();
        for (Map.Entry<List<UUID>, Integer> requestCount : requestCounts.entrySet()) {
            List<FilterStub> filterStubs = requestCount.getKey().stream()
                    .map(filterId -> new FilterStub(filterId, Objects.requireNonNull(filtersById.get(filterId))))
                    .toList();
            stubbedFilterRequests.add(new StubbedFilterRequest(stubStandaloneFilters(filterStubs), requestCount.getKey(), requestCount.getValue()));
        }
        return stubbedFilterRequests;
    }

    protected UUID stubStandaloneFilters(List<FilterStub> filterStubs) throws Exception {
        List<UUID> filterIds = filterStubs.stream().map(FilterStub::id).toList();
        String filterIdsQueryParam = filterIds.stream().map(UUID::toString).collect(Collectors.joining(","));
        return wireMockServer.stubFor(WireMock.get(WireMock.urlPathEqualTo(PATH))
                .withQueryParam("ids", WireMock.equalTo(filterIdsQueryParam))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filterStubs.stream().map(FilterStub::filter).toList()))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
    }

    protected void verifyStandaloneFiltersRequest(UUID stubId, List<UUID> filterIds) {
        verifyStandaloneFiltersRequest(stubId, filterIds, 1);
    }

    protected void verifyStandaloneFiltersRequest(UUID stubId, List<UUID> filterIds, int nbRequests) {
        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filterIds), false, nbRequests);
    }

    private void verifyStandaloneFiltersRequests(List<StubbedFilterRequest> stubs) {
        stubs.forEach(stub -> wireMockUtils.verifyGetRequest(stub.stubId(), PATH, handleQueryParams(stub.filterIds()), false, stub.requestCount()));
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
        return Map.of("ids", WireMock.equalTo(filterIds.stream().map(UUID::toString).collect(Collectors.joining(","))));
    }

    protected abstract void createEquipments();

    protected abstract Map<UUID, Set<String>> getFilterMapping();

    protected abstract List<FormulaInfos> getFormulaInfos();

    protected abstract List<FormulaInfos> getUpdatedFormulaInfos();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();
}
