/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.ResultActions;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author REHILI Ghazwa <ghazwarhili at gmail.com>
 */
@Tag("IntegrationTest")
class VoltageLevelTopologyModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfos = new ArrayList<>(
                Arrays.asList(
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1d1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1b1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1blcc")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1dlcc")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );
        return VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .activated(true)
                .equipmentId("v1")
                .equipmentAttributeModificationList(equipmentAttributeModificationInfos)
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfos = new ArrayList<>(
                Arrays.asList(
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1d1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(true)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build(),
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("v1b1")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(true)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );
        return VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .activated(true)
                .equipmentId("v1")
                .equipmentAttributeModificationList(equipmentAttributeModificationInfos)
                .build();
    }

    @Test
    void testInvalidVoltageLevel() throws Exception {
        VoltageLevelTopologyModificationInfos infos = VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .activated(true)
                .equipmentId("nonexistent_vl")
                .equipmentAttributeModificationList(List.of())
                .build();

        String body = getJsonBody(infos, null);
        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
                        .content(body)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn())).andExpect(status().isOk()).andReturn();
    }

    @Test
    void testEmptyModifications() throws Exception {
        List<EquipmentAttributeModificationInfos> emptyModifications = new ArrayList<>();

        VoltageLevelTopologyModificationInfos infos = VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .activated(true)
                .equipmentId("v1")
                .equipmentAttributeModificationList(emptyModifications)
                .build();

        String body = getJsonBody(infos, null);
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(body)
                        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    }

    @Test
    void testNonExistentSwitch() throws Exception {
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfos = new ArrayList<>(
                Arrays.asList(
                        EquipmentAttributeModificationInfos.builder()
                                .equipmentId("nonexistent_switch")
                                .equipmentAttributeName("open")
                                .equipmentAttributeValue(false)
                                .equipmentType(IdentifiableType.SWITCH)
                                .build()
                )
        );

        VoltageLevelTopologyModificationInfos infos = VoltageLevelTopologyModificationInfos.builder()
                .stashed(false)
                .equipmentId("v1")
                .equipmentAttributeModificationList(equipmentAttributeModificationInfos)
                .build();

        String body = getJsonBody(infos, null);
        ResultActions mockMvcResultActions = mockMvc.perform(post(getNetworkModificationUri())
                .content(body)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(request().asyncStarted());
        mockMvc.perform(asyncDispatch(mockMvcResultActions.andReturn())).andExpect(status().isOk()).andReturn();
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

}
