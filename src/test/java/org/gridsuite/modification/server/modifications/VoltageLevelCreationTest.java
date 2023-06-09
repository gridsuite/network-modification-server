/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CouplingDeviceInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.CREATE_VOLTAGE_LEVEL_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.SUBSTATION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.VOLTAGE_LEVEL_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.Assertions.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
@Tag("IntegrationTest")
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
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("1A").busbarSectionId2("1.A").build()))
                .build();
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

    @Test
    public void testCreateWithErrors() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setSubstationId("absent_station");

        String vliJson = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModification();
        vli.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        String vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Coupling between same bus bar section is not allowed").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModification();
        vli.setIpMin(0.0);
        vli.setIpMax(null);
        vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModificationUpdate();

        vli.setEquipmentId("");
        String vliJsonS2Object = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonS2Object).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", vli.getErrorType().name(), reportService);

        // try to create an existing VL
        vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("v1");
        vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, "v1").getMessage(),
                vli.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateWithBbsNotExist() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_1");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("bbs");
        String vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(getNetwork().getVoltageLevel("vl_1"));

        vli.setEquipmentId("vl_2");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("bbs");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        String vliJsonObject2 = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject2).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(getNetwork().getVoltageLevel("vl_2"));
    }

    public void testCreateWithShortCircuitExtension() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setIpMin(null);

        String vliJson = mapper.writeValueAsString(vli);
        mockMvc
                .perform(post(getNetworkModificationUri()).content(vliJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        VoltageLevelCreationInfos createdModification = (VoltageLevelCreationInfos) modificationRepository
                .getModifications(getGroupId(), false, true).get(0);
        assertThat(createdModification).recursivelyEquals(vli);

        vli.setIpMin(0.0);
        vli.setIpMax(null);
        vli.setEquipmentId("vlId2");

        vliJson = mapper.writeValueAsString(vli);
        mockMvc
                .perform(post(getNetworkModificationUri()).content(vliJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        createdModification = (VoltageLevelCreationInfos) modificationRepository
                .getModifications(getGroupId(), false, true).get(1);
        assertThat(createdModification).recursivelyEquals(vli);
    }
}
