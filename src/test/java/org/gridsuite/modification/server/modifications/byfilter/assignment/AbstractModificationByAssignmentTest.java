/**
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications.byfilter.assignment;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationByAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.AssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.DoubleAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.assignment.PropertyAssignmentInfos;
import org.gridsuite.modification.dto.byfilter.equipmentfield.PropertyField;
import org.gridsuite.modification.modifications.data.assignment.DataType;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.impacts.AbstractBaseImpact;
import org.gridsuite.modification.server.modifications.byfilter.AbstractByFilterTest;
import org.gridsuite.modification.server.service.FilterLoader;
import org.gridsuite.modification.server.service.FilterService;
import org.gridsuite.modification.server.utils.FilterStub;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import java.util.*;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.createCollectionElementImpact;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Thang PHAM <quyet-thang.pham at rte-france.com>
 */
@Tag("IntegrationTest")
abstract class AbstractModificationByAssignmentTest extends AbstractByFilterTest {
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

    @Override
    protected void assertResultImpacts(List<AbstractBaseImpact> impacts) {
        assertThat(impacts).containsExactly(createCollectionElementImpact(getIdentifiableType()));
    }

    @BeforeEach
    public void specificSetUp() {
        FilterService.setFilterServerBaseUri(wireMockServer.baseUrl());
        FilterLoader.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
        createEquipments();
    }

    @Test
    public void testByModificationError() throws Exception {
        //Test with modification = null
        mockMvc.perform(post(getNetworkModificationUri()).content(mapper.writeValueAsString(null)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isBadRequest());

        // Test with empty list of assignment
        checkCreationApplicationStatus(List.of(), NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with empty list of filters in assignment
        List<AssignmentInfos<?>> assignmentsWithNoFilters = getAssignmentInfos().stream().peek(assignmentInfos -> assignmentInfos.setFilters(List.of())).toList();
        checkCreationApplicationStatus(assignmentsWithNoFilters, NetworkModificationResult.ApplicationStatus.WITH_ERRORS);

        // Test with editedField = null
        AssignmentInfos<?> assignmentWithNoEditedField = DoubleAssignmentInfos.builder()
                .value(50.)
                .filters(List.of())
                .build();
        checkCreationApplicationStatus(List.of(assignmentWithNoEditedField), NetworkModificationResult.ApplicationStatus.WITH_ERRORS);
    }

    protected void checkCreateWithStatus(List<AssignmentInfos<?>> assignments, List<FilterStub> filterEquipments,
                                         NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        UUID stubId = stubStandaloneFilters(filterEquipments);

        checkCreationApplicationStatus(assignments, applicationStatus);

        verifyStandaloneFiltersRequest(stubId, filterEquipments.stream().map(FilterStub::id).collect(Collectors.toSet()));
    }

    @Test
    public void testModificationWithAllWrongEquipmentIds() throws Exception {
        FilterStub filter = createFilterStub(FILTER_WITH_ALL_WRONG_IDS, Set.of());

        List<AssignmentInfos<?>> assignmentsWithWrongFilter = getAssignmentInfos().stream()
                .peek(assignmentInfos -> assignmentInfos.setFilters(List.of(new FilterInfos(FILTER_WITH_ALL_WRONG_IDS, "filterWithWrongId"))))
                .toList();

        UUID stubId = stubStandaloneFilters(List.of(filter));

        checkCreationApplicationStatus(assignmentsWithWrongFilter, NetworkModificationResult.ApplicationStatus.WITH_WARNINGS);

        verifyStandaloneFiltersRequest(stubId, Set.of(FILTER_WITH_ALL_WRONG_IDS), getAssignmentInfos().size());
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(getAssignmentInfos().stream()
                .map(assignment -> assignment.getFilters().stream().map(FilterInfos::getId).toList())
                .toList());

        super.testCreate();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(getAssignmentInfos().stream()
                .map(assignment -> assignment.getFilters().stream().map(FilterInfos::getId).toList())
                .toList());

        super.testCopy();

        verifyStandaloneFiltersRequests(stubs);
    }

    protected void checkCreationApplicationStatus(List<? extends AssignmentInfos<?>> assignmentInfos,
                                                  NetworkModificationResult.ApplicationStatus applicationStatus) throws Exception {
        ModificationByAssignmentInfos modificationByAssignmentInfos = ModificationByAssignmentInfos.builder()
            .equipmentType(getIdentifiableType())
            .assignmentInfosList(assignmentInfos)
            .build();
        Optional<NetworkModificationsResult> networkModificationsResult;

        String bodyJson = getJsonBody(modificationByAssignmentInfos, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(bodyJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        MvcResult mvcResult = mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk()).andReturn();

        networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertTrue(networkModificationsResult.isPresent());
        assertEquals(1, extractApplicationStatus(networkModificationsResult.get()).size());
        assertEquals(applicationStatus, extractApplicationStatus(networkModificationsResult.get()).getFirst());
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

    protected abstract void createEquipments();

    protected List<AssignmentInfos<?>> getAssignmentInfos() {
        PropertyAssignmentInfos spyAssignmentInfos = spy(PropertyAssignmentInfos.builder()
                .editedField(PropertyField.FREE_PROPERTIES.name())
                .propertyName("propertyName")
                .value("propertyValue")
                .filters(List.of(filter1))
                .build());
        doReturn(DataType.PROPERTY).when(spyAssignmentInfos).getDataType();
        return new ArrayList<>(List.of(spyAssignmentInfos));
    }

    protected abstract List<AssignmentInfos<?>> getUpdatedAssignmentInfos();
}
