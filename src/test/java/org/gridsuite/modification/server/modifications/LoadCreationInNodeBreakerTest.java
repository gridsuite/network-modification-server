/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FreePropertyInfos;
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class LoadCreationInNodeBreakerTest extends AbstractNetworkModificationTest {
    private static String PROPERTY_NAME = "property-name";
    private static String PROPERTY_VALUE = "property-value";

    @Test
    public void testCreateWithErrors() throws Exception {
        // Create load without connection name and UNDEFINED direction
        LoadCreationInfos loadCreationInfos1 = (LoadCreationInfos) buildModification();
        loadCreationInfos1.setConnectionDirection(null);
        loadCreationInfos1.setConnectionName(null);
        String loadCreationInfosJson1 = mapper.writeValueAsString(loadCreationInfos1);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson1).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Technical error: java.lang.NullPointerException", loadCreationInfos1.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 1);

        // Equipment Id invalid
        LoadCreationInfos loadCreationInfos = (LoadCreationInfos) buildModification();
        loadCreationInfos.setEquipmentId("");
        String loadCreationInfosJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", loadCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 2);

        // VoltageLevel not found
        loadCreationInfos.setVoltageLevelId("notFoundVoltageLevelId");
        loadCreationInfosJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_NOT_FOUND, "notFoundVoltageLevelId").getMessage(),
                loadCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 3);

        loadCreationInfos.setEquipmentId("idLoad1");
        loadCreationInfos.setVoltageLevelId("v2");
        loadCreationInfos.setBusOrBusbarSectionId("notFoundBusbarSection");
        loadCreationInfosJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "notFoundBusbarSection").getMessage(),
                loadCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 4);

        loadCreationInfos.setBusOrBusbarSectionId("1B");
        loadCreationInfos.setP0(Double.NaN);
        loadCreationInfosJson = mapper.writeValueAsString(loadCreationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Load 'idLoad1': p0 is invalid", loadCreationInfos.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 5);

        loadCreationInfosJson = mapper.writeValueAsString(loadCreationInfos);
        MvcResult mvcResult = mockMvc.perform(post(getNetworkModificationUriWithBadVariant()).content(loadCreationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(status().isOk()).andReturn();
        Optional<NetworkModificationResult> networkModificationResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertNotNull(networkModificationResult);
        assertTrue(networkModificationResult.isEmpty());  // no modifications returned
        assertNull(getNetwork().getLoad("idLoad1"));  // load was not created
        testNetworkModificationsCount(getGroupId(), 6);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LoadCreationInfos.builder()
            .stashed(false)
            .active(true)
            .equipmentId("idLoad1")
            .equipmentName("nameLoad1")
            .voltageLevelId("v2")
            .busOrBusbarSectionId("1B")
            .loadType(LoadType.AUXILIARY)
            .p0(100.0)
            .q0(60.0)
            .connectionDirection(ConnectablePosition.Direction.TOP)
            .connectionName("top")
            .properties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()))
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LoadCreationInfos.builder()
            .stashed(false)
            .active(true)
            .equipmentId("idLoad1Edited")
            .equipmentName("nameLoad1Edited")
            .voltageLevelId("v2Edited")
            .busOrBusbarSectionId("1BEdited")
            .loadType(LoadType.AUXILIARY)
            .p0(200.0)
            .q0(90.0)
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .connectionName("topEdited")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
        assertEquals(PROPERTY_VALUE, getNetwork().getLoad("idLoad1").getProperty(PROPERTY_NAME));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLoad("idLoad1"));
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LOAD_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLoad1", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LOAD_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("idLoad1Edited", updatedValues.get("equipmentId"));
    }
}
