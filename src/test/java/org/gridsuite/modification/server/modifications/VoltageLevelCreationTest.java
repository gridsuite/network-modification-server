/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import lombok.SneakyThrows;
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
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
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
                .stashed(false)
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
    protected void assertAfterNetworkModificationCreation() {
        assertNotNull(getNetwork().getVoltageLevel("vlId"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_1_2"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_1"));
        assertNotNull(getNetwork().getBusbarSection("vlId_2_2"));
        assertTrue(getNetwork().getSubstation("s2").getVoltageLevelStream().anyMatch(vl -> vl.getId().equals("vlId")));
        assertEquals(1, getNetwork().getSubstation("s2").getVoltageLevelStream().filter(vl -> vl.getId().equals("vlId")).count());
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
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

    @Test
    public void testIpMinEqualsIpMax() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ok");
        vli.setIpMin(25.0);
        vli.setIpMax(25.0);
        String vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // VL is created
        assertNotNull(getNetwork().getVoltageLevel("vl_ok"));
    }

    private void testWithIccIsssue(Double ipMin, Double ipMax, String reportError) throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ko");
        vli.setIpMin(ipMin);
        vli.setIpMax(ipMax);
        String vliJsonObject = mapper.writeValueAsString(vli);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // VL could not have been created
        assertNull(getNetwork().getVoltageLevel("vl_ko"));
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, reportError).getMessage(), vli.getErrorType().name(), reportService);
    }

    @Test
    public void testIpMinGreaterThanIpMax() throws Exception {
        testWithIccIsssue(15.1, 15.0, "IpMin cannot be greater than IpMax");
    }

    @Test
    public void testIpMinNegative() throws Exception {
        testWithIccIsssue(-25.0, 15.0, "IpMin must be positive");
    }

    @Test
    public void testIpMaxNegative() throws Exception {
        testWithIccIsssue(25.0, -15.0, "IpMax must be positive");
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

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("VOLTAGE_LEVEL_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vlId", createdValues.get("equipmentId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("VOLTAGE_LEVEL_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("VoltageLevelIdEdited", updatedValues.get("equipmentId"));
    }
}
