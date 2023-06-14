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
import org.gridsuite.modification.server.dto.LoadCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Optional;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LoadCreationInNodeBreakerTest extends AbstractNetworkModificationTest {

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
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
        loadCreationInfos.setActivePower(Double.NaN);
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
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getLoad("idLoad1"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLoad("idLoad1"));
    }
}
