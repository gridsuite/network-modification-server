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
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CouplingDeviceInfos;
import org.gridsuite.modification.dto.FreePropertyInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.ModificationCreation;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author walid Sahnoun <walid.sahnoun at rte-france.com>
 */
@Tag("IntegrationTest")
class VoltageLevelCreationTest extends AbstractNetworkModificationTest {
    private static final String PROPERTY_NAME = "property-name";
    private static final String PROPERTY_VALUE = "property-value";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        VoltageLevelCreationInfos voltageLevel = ModificationCreation.getCreationVoltageLevel("s2", "vlId", "vlName");
        voltageLevel.setProperties(List.of(FreePropertyInfos.builder().name(PROPERTY_NAME).value(PROPERTY_VALUE).build()));
        return voltageLevel;
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return VoltageLevelCreationInfos.builder()
                .stashed(false)
                .equipmentId("VoltageLevelIdEdited")
                .equipmentName("VoltageLevelEdited")
                .substationId("s2")
                .nominalV(385)
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
        assertEquals(PROPERTY_VALUE, getNetwork().getVoltageLevel("vlId").getProperty(PROPERTY_NAME));
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
    void testCreateWithErrors() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setSubstationId("absent_station");

        String vliJson = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(SUBSTATION_NOT_FOUND, "absent_station").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModification();
        vli.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        String vliJsonObject = getJsonBody(vli, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "Coupling between same bus bar section is not allowed").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModification();
        vli.setIpMin(0.0);
        vli.setIpMax(null);
        vliJsonObject = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, "IpMax is required").getMessage(),
                vli.getErrorType().name(), reportService);

        vli = (VoltageLevelCreationInfos) buildModificationUpdate();

        vli.setEquipmentId("");
        String vliJsonS2Object = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonS2Object).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Invalid id ''", vli.getErrorType().name(), reportService);

        // try to create an existing VL
        vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("v1");
        vliJsonObject = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, "v1").getMessage(),
                vli.getErrorType().name(), reportService);
    }

    @Test
    void testCreateWithBbsNotExist() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_1");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("1.1");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("bbs");
        String vliJsonObject = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(getNetwork().getVoltageLevel("vl_1"));

        vli.setEquipmentId("vl_2");
        vli.getCouplingDevices().get(0).setBusbarSectionId1("bbs");
        vli.getCouplingDevices().get(0).setBusbarSectionId2("1.1");
        String vliJsonObject2 = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject2).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNotNull(getNetwork().getVoltageLevel("vl_2"));
    }

    @Test
    void testIpMinEqualsIpMax() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ok");
        vli.setIpMin(25.0);
        vli.setIpMax(25.0);
        String vliJsonObject = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // VL is created
        assertNotNull(getNetwork().getVoltageLevel("vl_ok"));
    }

    @Test
    void testCreateWithIpMinNull() throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ok");
        vli.setIpMin(null);
        vli.setIpMax(25.0);
        String vliJsonObject = getJsonBody(vli, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // VL is created
        assertNotNull(getNetwork().getVoltageLevel("vl_ok"));
    }

    private void testIccWithError(Double ipMin, Double ipMax, String reportError) throws Exception {
        VoltageLevelCreationInfos vli = (VoltageLevelCreationInfos) buildModification();
        vli.setEquipmentId("vl_ko");
        vli.setIpMin(ipMin);
        vli.setIpMax(ipMax);
        String vliJsonObject = getJsonBody(vli, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(vliJsonObject).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        // VL could not have been created
        assertNull(getNetwork().getVoltageLevel("vl_ko"));
        assertLogMessage(new NetworkModificationException(CREATE_VOLTAGE_LEVEL_ERROR, reportError).getMessage(), vli.getErrorType().name(), reportService);
    }

    @Test
    void testIpMinGreaterThanIpMax() throws Exception {
        testIccWithError(15.1, 15.0, "IpMin cannot be greater than IpMax");
    }

    @Test
    void testIpMinNegative() throws Exception {
        testIccWithError(-25.0, 15.0, "IpMin must be positive");
    }

    @Test
    void testIpMaxNegative() throws Exception {
        testIccWithError(25.0, -15.0, "IpMax must be positive");
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_CREATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("vlId", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("VOLTAGE_LEVEL_CREATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("VoltageLevelIdEdited", updatedValues.get("equipmentId"));
    }
}
