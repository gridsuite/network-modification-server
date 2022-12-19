/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentModificationInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherLoadCreationInfos;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LoadCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        // Create load without connection name and UNDEFINED direction
        LoadCreationInfos loadCreationInfos1 = (LoadCreationInfos) buildModification();
        loadCreationInfos1.setConnectionDirection(null);
        loadCreationInfos1.setConnectionName(null);
        String loadCreationInfosJson1 = objectWriter.writeValueAsString(loadCreationInfos1);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson1).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "java.lang.NullPointerException").getMessage())).andReturn();

        // Equipment Id invalid
        LoadCreationInfos loadCreationInfos = (LoadCreationInfos) buildModification();
        loadCreationInfos.setEquipmentId("");
        String loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "Invalid id ''").getMessage())).andReturn();

        // VoltageLevel not found
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage())).andReturn();

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is4xxClientError(), content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "Bus bar section notFoundBusbarSection not found").getMessage())).andReturn();

        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setActivePower(Double.NaN);
        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().is5xxServerError(), content().string(new NetworkModificationException(CREATE_LOAD_ERROR, "Load 'idLoad1': p0 is invalid").getMessage())).andReturn();

        //testNetworkModificationsCount(TEST_GROUP_ID, 0);

        loadCreationInfosJson = objectWriter.writeValueAsString(loadCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().isOk()).andReturn();
        String resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentModificationInfos> bsmlrModification = mapper.readValue(resultAsString, new TypeReference<>() { });

        assertTrue(bsmlrModification.isEmpty());  // no modifications returned
        assertNull(getNetwork().getLoad("idLoad1"));  // load was not created
        testNetworkModificationsCount(TEST_GROUP_ID, 1);  // new modification stored in the database
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
            .type(ModificationType.LOAD_CREATION)
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .loadType(LoadType.AUXILIARY)
            .activePower(100.0)
            .reactivePower(60.0)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .connectionName("top")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadCreationInfos.builder()
            .type(ModificationType.LOAD_CREATION)
            .equipmentId("idLoad1Edited")
            .equipmentName("nameLoad1Edited")
            .voltageLevelId("v2Edited")
            .busOrBusbarSectionId("1BEdited")
            .loadType(LoadType.AUXILIARY)
            .activePower(200.0)
            .reactivePower(90.0)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .connectionName("topEdited")
            .build();
    }

    @Override
    protected MatcherLoadCreationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherLoadCreationInfos.createMatcherLoadCreationInfos((LoadCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLoad("idLoad1"));
    }
}
