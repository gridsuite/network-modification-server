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
import org.gridsuite.modification.server.dto.BusbarConnectionCreationInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.MatcherVoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
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
                .nominalVoltage(385)
                .substationId("s2")
                .busbarSections(List.of())
                .busbarConnections(List.of())
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return MatcherVoltageLevelCreationInfos.createMatcherVoltageLevelCreationInfos((VoltageLevelCreationInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNotNull(getNetwork().getVoltageLevel("vlId"));
        assertNotNull(getNetwork().getBusbarSection("bbs.nw"));
        assertNotNull(getNetwork().getBusbarSection("bbs.ne"));
        assertNotNull(getNetwork().getBusbarSection("bbs.sw"));
        assertTrue(getNetwork().getSubstation("s2").getVoltageLevelStream().anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(1, getNetwork().getSubstation("s2").getVoltageLevelStream().filter(vl -> vl.getId().equals("vlId")).count());
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getVoltageLevel("vlId"));
        assertNull(getNetwork().getBusbarSection("bbs.nw"));
        assertNull(getNetwork().getBusbarSection("bbs.ne"));
        assertNull(getNetwork().getBusbarSection("bbs.sw"));
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
        vli.getBusbarConnections().add(BusbarConnectionCreationInfos.builder().
                fromBBS("bbs.ne").toBBS("bbs.ne").switchKind(SwitchKind.DISCONNECTOR).build());

        String vliJsonObject = mapper.writeValueAsString(vli);
        mvcResult = mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().is5xxServerError()).andReturn();
        resultAsString = mvcResult.getResponse().getContentAsString();
        assertEquals(resultAsString, new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Disconnector between same bus bar section 'bbs.ne'").getMessage());

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

    @SneakyThrows
    @Test
    public void testCreateWithConnectionErrors() {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        // try to create with wrong busbar as 'fromBBS'
        vli.setBusbarConnections(List.of(BusbarConnectionCreationInfos.builder().
                fromBBS("bbs.bad").toBBS("bbs.ne").switchKind(SwitchKind.DISCONNECTOR).build()));
        String vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_DEFINED, "bbs.bad").getMessage())
            );
        // same test with 'toBBS'
        vli.setBusbarConnections(List.of(BusbarConnectionCreationInfos.builder().
                fromBBS("bbs.ne").toBBS("bbs.wrong").switchKind(SwitchKind.DISCONNECTOR).build()));
        vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_DEFINED, "bbs.wrong").getMessage())
            );
    }
}
