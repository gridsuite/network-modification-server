/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.fasterxml.jackson.core.type.TypeReference;
import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.StringValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilter;
import org.gridsuite.filter.identifierlistfilter.IdentifierListFilterEquipmentAttributes;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.modification.server.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.server.dto.FilterInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.byfilter.DataType;
import org.gridsuite.modification.server.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.server.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Tag("IntegrationTest")
public abstract class AbstractModificationByAssignmentTest extends AbstractNetworkModificationTest {
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

    public static final String PATH = "/v1/filters/metadata";

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(getIdentifiableType()));
    }

    @Before
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());

        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    @Test
    public void testByModificationError() throws Exception {
        //Test with modification = null
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(null)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());

        // Test with empty list of simple modification
        checkCreationApplicationStatus(ModificationByAssignmentInfos.builder().equipmentType(getIdentifiableType()).assignmentInfosList(List.of()).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with empty list of filters in simple modification
        List<AssignmentInfos<?>> simpleInfosWithNoFilters = getAssignmentInfos().stream().peek(simpleInfos -> simpleInfos.setFilters(List.of())).toList();
        checkCreationApplicationStatus(ModificationByAssignmentInfos.builder().equipmentType(getIdentifiableType()).assignmentInfosList(simpleInfosWithNoFilters).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with editedField = null
        AssignmentInfos<?> simpleInfosWithNoEditedField = DoubleAssignmentInfos.builder()
                .value(50.)
                .filters(List.of())
                .build();
        checkCreationApplicationStatus(ModificationByAssignmentInfos.builder().equipmentType(getIdentifiableType()).assignmentInfosList(List.of(simpleInfosWithNoEditedField)).build(),
                NetworkModificationResult.ApplicationStatus.WITH_ERRORS);
    }

    protected void checkCreateWithWarning(List<AssignmentInfos<?>> simpleInfos, List<IdentifierListFilterEquipmentAttributes> existingEquipmentList) throws Exception {
        AbstractFilter filter = getFilterEquipments(FILTER_WITH_ONE_WRONG_ID, existingEquipmentList);

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + FILTER_WITH_ONE_WRONG_ID))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        ModificationByAssignmentInfos modificationByAssignmentInfos = ModificationByAssignmentInfos.builder()
                .assignmentInfosList(simpleInfos)
                .equipmentType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(modificationByAssignmentInfos, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(List.of(FILTER_WITH_ONE_WRONG_ID)), false);
    }

    protected void checkCreateWithError(List<AssignmentInfos<?>> simpleInfos, List<AbstractFilter> filterEquipments) throws Exception {
        String filterIds = filterEquipments.stream()
                .map(AbstractFilter::getId)
                .map(UUID::toString)
                .collect(Collectors.joining(","));

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + filterIds))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filterEquipments))
                        .withHeader("Content-Type", "application/json"))).getId();

        ModificationByAssignmentInfos modificationByAssignmentInfos = ModificationByAssignmentInfos.builder()
                .assignmentInfosList(simpleInfos)
                .equipmentType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(modificationByAssignmentInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId,
                PATH,
                handleQueryParams(filterEquipments.stream().map(AbstractFilter::getId).collect(Collectors.toList())),
                false);
    }

    @Test
    public void testModificationWithAllWrongEquipmentIds() throws Exception {
        AbstractFilter filter = getFilterEquipments(FILTER_WITH_ALL_WRONG_IDS, List.of());

        List<AssignmentInfos<?>> simpleInfosList = getAssignmentInfos().stream()
                .peek(simpleInfos -> simpleInfos.setFilters(List.of(new FilterInfos(FILTER_WITH_ALL_WRONG_IDS, "filterWithWrongId"))))
                .toList();

        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching("/v1/filters/metadata\\?ids=" + FILTER_WITH_ALL_WRONG_IDS))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(List.of(filter)))
                        .withHeader("Content-Type", "application/json"))).getId();

        ModificationByAssignmentInfos modificationByAssignmentInfos = ModificationByAssignmentInfos.builder()
                .assignmentInfosList(simpleInfosList)
                .equipmentType(getIdentifiableType())
                .build();

        checkCreationApplicationStatus(modificationByAssignmentInfos, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(List.of(FILTER_WITH_ALL_WRONG_IDS)), false);
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<AbstractFilter> filters = getTestFilters();
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + ".{2,}"))
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
        UUID stubId = wireMockServer.stubFor(WireMock.get(WireMock.urlMatching(getPath(true) + ".{2,}"))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filters))
                        .withHeader("Content-Type", "application/json"))).getId();

        super.testCopy();

        wireMockUtils.verifyGetRequest(stubId, PATH, handleQueryParams(filters.stream().map(AbstractFilter::getId).collect(Collectors.toList())), false);
    }

    protected void checkCreationApplicationStatus(ModificationByAssignmentInfos modificationByAssignmentInfos,
                                                  NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        String modificationToCreateJson = mapper.writeValueAsString(modificationByAssignmentInfos);

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
    protected ModificationByAssignmentInfos buildModification() {
        return ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(getAssignmentInfos())
                .stashed(false)
                .build();
    }

    @Override
    protected ModificationByAssignmentInfos buildModificationUpdate() {
        return ModificationByAssignmentInfos.builder()
                .equipmentType(getIdentifiableType())
                .assignmentInfosList(getUpdatedAssignmentInfos())
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

    Map<String, StringValuePattern> handleQueryParams(List<UUID> filterIds) {
        return Map.of("ids", WireMock.matching(filterIds.stream().map(uuid -> ".+").collect(Collectors.joining(","))));
    }

    String getPath(boolean isRegexPath) {
        if (isRegexPath) {
            return "/v1/filters/metadata\\?ids=";
        }
        return "/v1/filters/metadata?ids=";
    }

    protected abstract void createEquipments();

    protected abstract List<AbstractFilter> getTestFilters();

    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        PropertyAssignmentInfos spySimpleInfos = spy(PropertyAssignmentInfos.builder()
                .editedField(DataType.PROPERTY.name())
                .propertyName("propertyName")
                .value("propertyValue")
                .filters(List.of(filter1))
                .build());
        doReturn(DataType.PROPERTY).when(spySimpleInfos).getDataType();
        return new ArrayList<>(List.of(spySimpleInfos));
    }

    protected abstract List<AssignmentInfos<?>> getUpdatedAssignmentInfos();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();
}
