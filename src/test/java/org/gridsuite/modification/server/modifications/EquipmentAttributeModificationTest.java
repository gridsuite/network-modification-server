/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos;
import org.gridsuite.modification.server.utils.MatcherEquipmentModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class EquipmentAttributeModificationTest extends AbstractNetworkModificationTest {

    @Override
    protected UUID getNetworkUuid() {
        return TEST_NETWORK_ID;
    }

    @SneakyThrows
    @Test
    public void testEquipmentAttributeModificationInfos() {
        MvcResult mvcResult;
        String resultAsString;
        UUID modificationUuid = UUID.randomUUID();
        EquipmentAttributeModificationInfos modificationInfos = EquipmentAttributeModificationInfos.builder()
            .uuid(modificationUuid)
            .date(ZonedDateTime.of(2021, 2, 19, 0, 0, 0, 0, ZoneOffset.UTC))
            .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
            .equipmentId("equipmentId")
            .substationIds(Set.of("substationId"))
            .equipmentAttributeName("equipmentAttributeName")
            .equipmentAttributeValue("equipmentAttributeValue")
            .equipmentType(IdentifiableType.VOLTAGE_LEVEL)
            .build();
        assertEquals(String.format("EquipmentAttributeModificationInfos(super=EquipmentModificationInfos(super=ModificationInfos(uuid=%s, date=2021-02-19T00:00Z, type=EQUIPMENT_ATTRIBUTE_MODIFICATION, substationIds=[substationId]), equipmentId=equipmentId), equipmentAttributeName=equipmentAttributeName, equipmentAttributeValue=equipmentAttributeValue, equipmentType=VOLTAGE_LEVEL)", modificationUuid), modificationInfos.toString());

        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);

        // switch opening
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfosList = mapper.readValue(resultAsString, new TypeReference<>() { });
        EquipmentAttributeModificationInfos modificationSwitchInfos =
            Objects.requireNonNull(equipmentAttributeModificationInfosList).get(0);

        org.hamcrest.MatcherAssert.assertThat(modificationSwitchInfos, MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("v1b1", Set.of("s1"), "open", true, IdentifiableType.SWITCH));

        // switch in variant VARIANT_ID opening
        switchStatusModificationInfos.setEquipmentId("break1Variant");
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + "&variantId=" + NetworkCreation.VARIANT_ID).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> equipmentAttributeModificationInfosListSwitch = mapper.readValue(resultAsString, new TypeReference<>() { });
        modificationSwitchInfos = Objects.requireNonNull(equipmentAttributeModificationInfosListSwitch).get(0);

        org.hamcrest.MatcherAssert.assertThat(modificationSwitchInfos, MatcherEquipmentAttributeModificationInfos.createMatcherEquipmentAttributeModificationInfos("break1Variant", Set.of("s1Variant"), "open", true, IdentifiableType.SWITCH));
    }

    @Test
    public void testSwitch() {
        // switches modifications on initial variant
        switchModifications("", "v1b1", "disc1Variant", "v2b1", "v3b1", Set.of("s1"), Set.of("s2"), 4);

        // switches modifications on variant VARIANT_ID
        switchModifications("&variantId=" + NetworkCreation.VARIANT_ID, "break1Variant", "notFound", "disc1Variant", "break2Variant", Set.of("s1Variant"), Set.of("s2Variant"), 8);
    }

    @SneakyThrows
    private void switchModifications(String extraParams, String switchId1, String switchNotFoundId, String switchId2, String switchId3,
                                     Set<String> substationsIds, Set<String> otherSubstationsIds,
                                     int modificationsCount) {
        MvcResult mvcResult;
        String resultAsString;

        EquipmentAttributeModificationInfos switchStatusModificationInfos = buildModification();

        // switch not existing
        switchStatusModificationInfos.setEquipmentId(switchNotFoundId);
        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpectAll(
            status().isNotFound(),
            content().string(new NetworkModificationException(EQUIPMENT_NOT_FOUND, switchNotFoundId).getMessage()));

        // switch closing when already closed
        switchStatusModificationInfos.setEquipmentId(switchId1);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultAttributeModificationInfos = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultAttributeModificationInfos.get(0), createMatcherEquipmentAttributeModificationInfos(switchId1, Set.of(), "open", false, IdentifiableType.SWITCH));

        // switch opening
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultSwitchOpening = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultSwitchOpening.get(0), createMatcherEquipmentAttributeModificationInfos(switchId1, substationsIds, "open", true, IdentifiableType.SWITCH));
        // switch closing
        switchStatusModificationInfos.setEquipmentId(switchId2);
        switchStatusModificationInfos.setEquipmentAttributeValue(false);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultami = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultami.get(0), createMatcherEquipmentAttributeModificationInfos(switchId2, substationsIds, "open", false, IdentifiableType.SWITCH));

        // switch opening on another substation
        switchStatusModificationInfos.setEquipmentId(switchId3);
        switchStatusModificationInfos.setEquipmentAttributeValue(true);
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri() + extraParams).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        List<EquipmentAttributeModificationInfos> bsiListResultAttributemi = mapper.readValue(resultAsString, new TypeReference<>() { });
        assertThat(bsiListResultAttributemi.get(0), createMatcherEquipmentAttributeModificationInfos(switchId3, otherSubstationsIds, "open", true, IdentifiableType.SWITCH));

        testNetworkModificationsCount(TEST_GROUP_ID, modificationsCount);
    }

    @SneakyThrows
    @Test
    public void testWithErrors() {
        // bad equipment attribute name
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
            .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("close") // bad
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();

        String switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_NAME_ERROR, "For switch status, the attribute name is only 'open'").getMessage()));

        // bad equipment attribute value
        switchStatusModificationInfos.setEquipmentAttributeName("open");
        switchStatusModificationInfos.setEquipmentAttributeValue("opened"); // bad
        switchStatusModificationInfosJson = objectWriter.writeValueAsString(switchStatusModificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(switchStatusModificationInfosJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().isBadRequest(),
                content().string(new NetworkModificationException(EQUIPMENT_ATTRIBUTE_VALUE_ERROR, "For switch status, the attribute values are only " + Set.of(true, false)).getMessage()));
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModification() {
        return EquipmentAttributeModificationInfos.builder()
            .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(true)
            .equipmentId("v1b1")
            .build();
    }

    @Override
    protected EquipmentAttributeModificationInfos buildModificationUpdate() {
        return EquipmentAttributeModificationInfos.builder()
            .type(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION)
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(false)
            .equipmentId("v1b1Edited")
            .build();
    }

    @Override
    protected MatcherEquipmentModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherEquipmentModificationInfos.createMatcherEquipmentModificationInfos((EquipmentAttributeModificationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertTrue(getNetwork().getSwitch("v1b1").isOpen());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertFalse(getNetwork().getSwitch("v1b1").isOpen());
    }
}
