/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;

import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CouplingDeviceInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.MatcherVoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Arrays;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
public class VoltageLevelCreationTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return ModificationCreation.getCreationVoltageLevel("s2", "vlId", "vlName");
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageLevelCreationInfos.builder()
                .equipmentId("VoltageLevelIdEdited")
                .equipmentName("VoltageLevelEdited")
                .substationId("s2")
                .nominalVoltage(385)
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("bbs.nw").busbarSectionId2("bbs.ne").build()))
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos((VoltageLevelCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getVoltageLevel("vlId"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_2"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_2"));
        assertTrue(getNetwork().getSubstation("s2").getVoltageLevelStream().anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(1, getNetwork().getSubstation("s2").getVoltageLevelStream().filter(vl -> vl.getId().equals("vlId")).count());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getVoltageLevel("vlId"));
        assertNull(getNetwork().getBusbarSection("vlId_1_1"));
        assertNull(getNetwork().getBusbarSection("vlId_1_2"));
        assertNull(getNetwork().getBusbarSection("vlId_2_1"));
        assertNull(getNetwork().getBusbarSection("vlId_2_2"));
        assertFalse(getNetwork().getSubstation("s2").getVoltageLevelStream().anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(0, getNetwork().getSubstation("s2").getVoltageLevelStream().filter(vl -> vl.getId().equals("vlId")).count());
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        MvcResult mvcResult;
        String resultAsString;

        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setSubstationId("absent_station");

        String vliJson = mapper.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(vliJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is4xxClientError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage());

        vli = (VoltageLevelCreationInfos) buildModification();
        vli.getCouplingDevices().get(0).setBusbarSectionId1("bbs.ne");
        String vliJsonObject = mapper.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Coupling between same bus bar section is not allowed").getMessage());

        vli = (VoltageLevelCreationInfos) buildModificationUpdate();

        vli.setEquipmentId("");
        String vliJsonS2Object = mapper.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonS2Object).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Invalid id ''").getMessage());

        // try to create an existing VL
        vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("v1");
        vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, "v1").getMessage())
            );
    }

}
