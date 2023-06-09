/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.testElementModificationImpact;
import static org.gridsuite.modification.server.Impacts.TestImpactUtils.testEmptyImpacts;
import static org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class EquipmentAttributeModificationTest extends AbstractNetworkModificationTest {

    @SneakyThrows
    @Test
    public void testEquipmentAttributeModificationInfos() {
        MvcResult mvcResult;
        UUID modificationUuid = UUID.randomUUID();
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder()
            .uuid(modificationUuid)
            .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
            .equipmentId("equipmentId")
            .equipmentAttributeName("equipmentAttributeName")
            .equipmentAttributeValue("equipmentAttributeValue")
            .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
            .build();
        assertEquals(String.format("EquipmentAttributeModificationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=%s, date=2021-02-19T00:00Z), equipmentId=equipmentId), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue, equipmentType=VOLTAGE_LEVEL)", modificationUuid), modificationInfos.toString());

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
        String switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);

        // switch opening
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, "v1b1", Set.of("s1"));

        // switch in variant VARIANT_ID opening
        switchStatusModificationInfos.setEquipmentId("break1Variant");
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + "&variantId=" + NetworkCreation.VARIANT_ID).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, "break1Variant", Set.of("s1Variant"));
    }

    @Test
    public void testSwitch() {
        // switches modifications on initial variant
        switchModifications("", "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 5);

        // switches modifications on variant VARIANT_ID
        switchModifications("&variantId=" + NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 10);
    }

    @SneakyThrows
    private void switchModifications(String extraParams, String switchId1, String switchNotFoundId, String switchId2, String switchId3,
                                     Set<String> substationsIds, Set<String> otherSubstationsIds,
                                     int modificationsCount) {
        MvcResult mvcResult;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = buildModification();

        // switch not existing
        switchStatusModificationInfos.setEquipmentId(switchNotFoundId);
        String switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, switchNotFoundId).getMessage(),
                switchStatusModificationInfos.getErrorType().name(), reportService);

        // switch closing when already closed
        switchStatusModificationInfos.setEquipmentId(switchId1);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testEmptyImpacts(mapper, mvcResult.getResponse().getContentAsString());

        // switch opening
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, switchId1, substationsIds);

        // switch closing
        switchStatusModificationInfos.setEquipmentId(switchId2);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, switchId2, substationsIds);

        // switch opening on another substation
        switchStatusModificationInfos.setEquipmentId(switchId3);
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        testElementModificationImpact(mapper, mvcResult.getResponse().getContentAsString(), IdentifiableType.SWITCH, switchId3, otherSubstationsIds);

        testNetworkModificationsCount(getGroupId(), modificationsCount);
    }

    @SneakyThrows
    @Test
    public void testWithErrors() {
        // bad equipment attribute name
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("close") // bad
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();

        String switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'").getMessage()));

        // bad equipment attribute value
        switchStatusModificationInfos.setEquipmentAttributeName("open");
        switchStatusModificationInfos.setEquipmentAttributeValue("opened"); // bad
        switchStatusModificationInfosJson = mapper.writeValueAsString(switchStatusModificationInfos);
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
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModificationUpdate() {
        return EquipmentAttributeModificationInfos.builder()
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(false)
            .equipmentId("v1b1Edited")
            .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherEquipmentAttributeModificationInfos((EquipmentAttributeModificationInfos) modificationInfos);
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertTrue(getNetwork().getSwitch("v1b1").isOpen());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertFalse(getNetwork().getSwitch("v1b1").isOpen());
    }
}
