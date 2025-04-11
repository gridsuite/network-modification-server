/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.testElementModificationImpact;
import static org.gridsuite.modification.server.impacts.TestImpactUtils.testEmptyImpacts;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class EquipmentAttributeModificationTest extends AbstractNetworkModificationTest {

    @Test
    void testEquipmentAttributeModificationInfos() throws Exception {
        MvcResult mvcResult;
        UUID modificationUuid = UUID.randomUUID();
        //We need to limit the precision to avoid database precision storage limit issue (postgres has a precision of 6 digits while h2 can go to 9)
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .uuid(modificationUuid)
            .date(Instant.parse("2021-02-19T00:00:00Z"))
            .equipmentId("equipmentId")
            .equipmentAttributeName("equipmentAttributeName")
            .equipmentAttributeValue("equipmentAttributeValue")
            .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
            .build();
        assertEquals(String.format("EquipmentAttributeModificationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=%s, type=EQUIPMENT_ATTRIBUTE_MODIFICATION, date=2021-02-19T00:00:00Z, stashed=false, messageType=null, messageValues=null, activated=true), equipmentId=equipmentId, properties=null), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue, equipmentType=VOLTAGE_LEVEL)", modificationUuid), modificationInfos.toString());

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
        String switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext())));

        // switch opening
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1"));

        // switch in variant VARIANT_ID opening
        switchStatusModificationInfos.setEquipmentId("break1Variant");
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(NetworkCreation.VARIANT_ID))));
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), Set.of("s1Variant"));
    }

    @Test
    void testSwitch() throws Exception {
        // switches modifications on initial variant
        switchModifications(null, "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 5);

        // switches modifications on variant VARIANT_ID
        switchModifications(NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 10);
    }

    private void switchModifications(String extraParams, String switchId1, String switchNotFoundId, String switchId2, String switchId3,
                                     Set<String> substationsIds, Set<String> otherSubstationsIds,
                                     int modificationsCount) throws Exception {
        MvcResult mvcResult;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = buildModification();

        // switch not existing
        switchStatusModificationInfos.setEquipmentId(switchNotFoundId);
        String switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(extraParams))));
        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, switchNotFoundId).getMessage(),
                switchStatusModificationInfos.getErrorType().name(), reportService);

        // switch closing when already closed
        switchStatusModificationInfos.setEquipmentId(switchId1);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(extraParams))));
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testEmptyImpacts(mapper, mvcResult.getResponse().getContentAsString());

        // switch opening
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(extraParams))));
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), substationsIds);

        // switch closing
        switchStatusModificationInfos.setEquipmentId(switchId2);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(extraParams))));
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), substationsIds);

        // switch opening on another substation
        switchStatusModificationInfos.setEquipmentId(switchId3);
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext(extraParams))));
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), otherSubstationsIds);

        testNetworkModificationsCount(getGroupId(), modificationsCount);
    }

    @Test
    void testWithErrors() throws Exception {
        // bad equipment attribute name
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("close") // bad
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();

        String switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext())));
        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'").getMessage()));

        // bad equipment attribute value
        switchStatusModificationInfos.setEquipmentAttributeName("open");
        switchStatusModificationInfos.setEquipmentAttributeValue("opened"); // bad
        switchStatusModificationInfosJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(switchStatusModificationInfos, List.of(buildApplicationContext())));

        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_VALUE_ERROR, "For switch status, the attribute values are only " + Set.of(true, false)).getMessage()));
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModification() {
        return EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModificationUpdate() {
        return EquipmentAttributeModificationInfos.builder()
            .stashed(false)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(false)
            .equipmentId("v1b1Edited")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertTrue(getNetwork().getSwitch("v1b1").isOpen());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertFalse(getNetwork().getSwitch("v1b1").isOpen());
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("EQUIPMENT_ATTRIBUTE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("open", createdValues.get("equipmentAttributeName"));
        assertEquals("v1b1", createdValues.get("equipmentId"));
        assertEquals("true", createdValues.get("equipmentAttributeValue"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("EQUIPMENT_ATTRIBUTE_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("open", createdValues.get("equipmentAttributeName"));
        assertEquals("v1b1Edited", createdValues.get("equipmentId"));
        assertEquals("false", createdValues.get("equipmentAttributeValue"));
    }
}
