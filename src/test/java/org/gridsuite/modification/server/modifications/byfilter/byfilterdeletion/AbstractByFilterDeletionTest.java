/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter.byfilterdeletion;

import com.fasterxml.jackson.core.type.TypeReference;
import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.modifications.byfilter.AbstractByFilterTest;
import org.gridsuite.modification.server.service.FilterLoader;
import org.gridsuite.modification.server.utils.FilterStub;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

import java.util.*;

import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

abstract class AbstractByFilterDeletionTest extends AbstractByFilterTest {
    protected static final UUID FILTER_ID_1 = UUID.randomUUID();
    protected static final UUID FILTER_ID_2 = UUID.randomUUID();
    protected static final UUID FILTER_ID_3 = UUID.randomUUID();

    protected static final String EQUIPMENT_WRONG_ID_1 = "wrongId1";

    @BeforeEach
    public void specificSetUp() {
        FilterLoader.setFilterServerBaseUri(wireMockServer.baseUrl());
        getNetwork().getVariantManager().setWorkingVariant("variant_1");
    }

    @Test
    @Override
    public void testCreate() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(List.of(getFilterMapping().keySet().stream().toList()));

        super.testCreate();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Test
    @Override
    public void testCopy() throws Exception {
        List<StubbedFilterRequest> stubs = stubStandaloneFilterRequests(List.of(getFilterMapping().keySet().stream().toList()));

        super.testCopy();

        verifyStandaloneFiltersRequests(stubs);
    }

    @Test
    void testCreateAllFiltersWrong() throws Exception {
        var filter1 = FilterInfos.builder()
                .id(FILTER_ID_1)
                .name("filter1")
                .build();
        ByFilterDeletionInfos byFilterDeletionInfos = ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter1))
                .build();
        FilterStub filterStub = createFilterStub(FILTER_ID_1, Set.of(EQUIPMENT_WRONG_ID_1));
        UUID stubId = stubStandaloneFilters(List.of(filterStub));
        String body = getJsonBody(byFilterDeletionInfos, null);

        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri()).content(body).contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn()))
                .andExpect(status().isOk());
        assertLogMessage("No equipment will be removed",
                "network.modification.byFilterDeletion.noEquipmentToRemove", reportService);
        verifyStandaloneFiltersRequest(stubId, Set.of(FILTER_ID_1));
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        var filter2 = FilterInfos.builder()
                .id(FILTER_ID_2)
                .name("filter 2 modified")
                .build();

        return ByFilterDeletionInfos.builder()
                .stashed(false)
                .equipmentType(getIdentifiableType())
                .filters(List.of(filter2))
                .build();
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(getIdentifiableType().name(), createdValues.get("equipmentType"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("BY_FILTER_DELETION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals(getIdentifiableType().name(), createdValues.get("equipmentType"));
    }
}
