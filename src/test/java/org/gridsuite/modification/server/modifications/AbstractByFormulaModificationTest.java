/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.dto.ByFormulaModificationInfos;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.IdentifiableAttributes;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.formula.FormulaInfos;
import org.gridsuite.modification.server.dto.formula.Operator;
import org.gridsuite.modification.server.dto.formula.ReferenceFieldOrValue;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Seddik Yengui <Seddik.yengui at rte-france.com>
 */

@Tag("IntegrationTest")
public abstract class AbstractByFormulaModificationTest extends AbstractNetworkModificationTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final UUID FILTER_ID_3 = UUID.randomUUID();
    protected static final UUID FILTER_ID_4 = UUID.randomUUID();
    protected static final UUID FILTER_ID_5 = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ALL_WRONG_IDS = UUID.randomUUID();
    protected static final UUID FILTER_WITH_ONE_WRONG_ID = UUID.randomUUID();
    protected final FilterInfos filter1 = new FilterInfos(FILTER_ID_1, "filter1");
    protected final FilterInfos filter2 = new FilterInfos(FILTER_ID_2, "filter2");
    protected final FilterInfos filter3 = new FilterInfos(FILTER_ID_3, "filter3");
    protected final FilterInfos filter4 = new FilterInfos(FILTER_ID_4, "filter4");
    protected final FilterInfos filter5 = new FilterInfos(FILTER_ID_5, "filter5");
    protected final FilterInfos filterWithAllWrongId = new FilterInfos(FILTER_WITH_ALL_WRONG_IDS, "filterWithAllWrongId");
    protected final FilterInfos filterWithOneWrongId = new FilterInfos(FILTER_WITH_ONE_WRONG_ID, "filterWithOneWrongId");

    public static final String PATH = "/v1/filters/export";

    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    public void checkCreateWithWarning(List<FormulaInfos> formulaInfos, List<IdentifiableAttributes> existingEquipmentList) throws Exception {
        FilterEquipments filter = getFilterEquipments(FILTER_WITH_ONE_WRONG_ID, "filterWithWrongId", existingEquipmentList, List.of("wrongId"));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/export\\?networkUuid=" + getNetworkUuid() + "&variantId=variant_1&ids=" + FILTER_WITH_ONE_WRONG_ID))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), List.of(FILTER_WITH_ONE_WRONG_ID)), false);
    }

    public void checkCreateWithError(List<FormulaInfos> formulaInfos) throws Exception {
        FilterEquipments filter = getFilterEquipments(FILTER_WITH_ALL_WRONG_IDS, "filterWithWrongId", List.of(), List.of("wrongId1", "wrongId2"));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/export\\?networkUuid=" + getNetworkUuid() + "&variantId=variant_1&ids=" + FILTER_WITH_ALL_WRONG_IDS))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        ByFormulaModificationInfos byFormulaModificationInfos = ByFormulaModificationInfos.builder()
                .formulaInfosList(formulaInfos)
                .identifiableType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(byFormulaModificationInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), List.of(FILTER_WITH_ALL_WRONG_IDS)), false);
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCreate();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    @Test
    @Override
    public void testCopy() throws Exception {

        List<FilterEquipments> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(getNetworkUuid(), true) + "(.+,){4}.*"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(getNetworkUuid(), filters.stream().map(FilterEquipments::getFilterId).collect(Collectors.toList())), false);
    }

    void checkCreationApplicationStatus(ByFormulaModificationInfos byFormulaModificationInfos,
                                        NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        String modificationToCreateJson = mapper.writeValueAsString(byFormulaModificationInfos);

        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(modificationToCreateJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationResult.isPresent());
        assertEquals(applicationStatus, networkModificationResult.get().getApplicationStatus());
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

    IdentifiableAttributes getIdentifiableAttributes(String id, Double distributionKey) {
        return IdentifiableAttributes.builder()
                .id(id)
                .type(getIdentifiableType())
                .distributionKey(distributionKey)
                .build();
    }

    FilterEquipments getFilterEquipments(UUID filterID, String filterName, List<IdentifiableAttributes> identifiableAttributes, List<String> notFoundEquipments) {
        return FilterEquipments.builder()
                .filterId(filterID)
                .filterName(filterName)
                .identifiableAttributes(identifiableAttributes)
                .notFoundEquipments(notFoundEquipments)
                .build();
    }

    FormulaInfos getFormulaInfo(String editedField,
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

    Map<String, StringValuePattern> handleQueryParams(UUID networkUuid, List<UUID> filterIds) {
        return Map.of("networkUuid", WireMock.equalTo(String.valueOf(networkUuid)), "variantId", WireMock.equalTo("variant_1"), "ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    private String getPath(UUID networkUuid, boolean isRegexPhat) {
        if (isRegexPhat) {
            return "/v1/filters/export\\?networkUuid=" + networkUuid + "\\&variantId=variant_1\\&ids=";
        }
        return "/v1/filters/export?networkUuid=" + networkUuid + "&variantId=variant_1&ids=";
    }

    abstract void createEquipments();

    abstract List<FilterEquipments> getTestFilters();

    abstract List<FormulaInfos> getFormulaInfos();

    abstract List<FormulaInfos> getUpdatedFormulaInfos();

    abstract IdentifiableType getIdentifiableType();
}
