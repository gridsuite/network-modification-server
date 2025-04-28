/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.Switch;
import com.powsybl.iidm.network.VoltageLevel;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelTopologyModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author REHILI Ghazwa <ghazwarhili@gmail.com>
 */
@Tag("IntegrationTest")
class VoltageLevelTopologyModificationTest extends AbstractNetworkModificationTest {
    private ObjectMapper objectMapper;

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

    @Override
    protected void assertAfterNetworkModificationCreation() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        assertNotNull(voltageLevel);

        Switch switch1 = getNetwork().getSwitch("v1d1");
        assertNotNull(switch1);
        assertFalse(switch1.isOpen());

        Switch switch2 = getNetwork().getSwitch("v1b1");
        assertNotNull(switch2);
        assertFalse(switch2.isOpen());

        Switch switch3 = getNetwork().getSwitch("v1blcc");
        assertNotNull(switch3);
        assertFalse(switch3.isOpen());

        Switch switch4 = getNetwork().getSwitch("v1dlcc");
        assertNotNull(switch4);
        assertFalse(switch4.isOpen());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        VoltageLevel voltageLevel = getNetwork().getVoltageLevel("v1");
        assertNotNull(voltageLevel);

        Switch switch1 = getNetwork().getSwitch("v1d1");
        assertNotNull(switch1);
        assertFalse(switch1.isOpen());

        Switch switch2 = getNetwork().getSwitch("v1b1");
        assertNotNull(switch2);
        assertFalse(switch2.isOpen());

        Switch switch3 = getNetwork().getSwitch("v1blcc");
        assertNotNull(switch3);
        assertFalse(switch3.isOpen());

        Switch switch4 = getNetwork().getSwitch("v1dlcc");
        assertNotNull(switch4);
        assertFalse(switch4.isOpen());
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
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(body)
                        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
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
        mockMvc.perform(post(getNetworkModificationUri())
                .content(body)
                .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    }

    private void applyModification(VoltageLevelTopologyModificationInfos infos) throws Exception {
        String body = getJsonBody(infos, null);
        mockMvc.perform(post(getNetworkModificationUri())
                        .content(body)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("v1", updatedValues.get("equipmentId"));
    }
}
