/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Line;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import com.powsybl.iidm.network.VoltageLevel;
import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.NetworkUtil.createSwitch;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class OperatingStatusModificationLockoutLineTest extends AbstractNetworkModificationTest {
    private static final String TARGET_LINE_ID = "line2";
    private static final String UPDATE_BRANCH_ID = "line1";
    private static final OperatingStatus.Status TARGET_BRANCH_STATUS = PLANNED_OUTAGE;
    private static final OperatingStatus.Status OTHER_BRANCH_STATUS = FORCED_OUTAGE;

    @Override
    protected Network createNetwork(UUID networkUuid) {
        Network network = NetworkCreation.createSwitchNetwork(networkUuid, new NetworkFactoryImpl());
        // force a branch status different from the expected one, after testCreate
        TestUtils.setOperatingStatus(network, TARGET_LINE_ID, OTHER_BRANCH_STATUS);
        return network;
    }

    private Line createLineAndSwitches(SwitchKind switchKind, boolean isFictitious) {
        VoltageLevel vl2 = getNetwork().getVoltageLevel("vl2");
        createSwitch(vl2, "br12", "br12", switchKind, false, false, isFictitious, 0, 3);
        createSwitch(vl2, "br22", "br22", switchKind, false, false, isFictitious, 0, 3);

        return getNetwork().newLine()
                .setId("line1")
                .setName("line1")
                .setVoltageLevel1("vl1")
                .setVoltageLevel2("vl2")
                .setR(0.1)
                .setX(10.0)
                .setG1(0.0)
                .setG2(0.0)
                .setB1(0.0)
                .setB2(0.0)
                .setNode1(3)
                .setNode2(3)
                .add();
    }

    @Override
    protected ModificationInfos buildModification() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(TARGET_LINE_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.LOCKOUT).build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return OperatingStatusModificationInfos.builder()
                .stashed(false)
                .equipmentId(UPDATE_BRANCH_ID)
                .energizedVoltageLevelId("energizedVoltageLevelId")
                .action(OperatingStatusModificationInfos.ActionType.SWITCH_ON).build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_LINE_ID, TARGET_BRANCH_STATUS);
    }

    private void testLockoutLine(String lineID) throws Exception {
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId(lineID);
        modificationInfos.setAction(OperatingStatusModificationInfos.ActionType.LOCKOUT);
        String modificationJson = mapper.writeValueAsString(modificationInfos);
        assertNull(getNetwork().getLine(lineID).getExtension(OperatingStatus.class));

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        TestUtils.assertOperatingStatus(getNetwork(), lineID, TARGET_BRANCH_STATUS);
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        // go back to init status
        TestUtils.assertOperatingStatus(getNetwork(), TARGET_LINE_ID, OTHER_BRANCH_STATUS);
    }

    @Test
    void testLockoutLinesWithLoadBreakerSwitches() throws Exception {
        //Lockout line with switches of kind LOAD_BREAK_SWITCH
        createLineAndSwitches(SwitchKind.LOAD_BREAK_SWITCH, false);
        testLockoutLine("line1");
    }

    @Test
    void testLockoutLinesWithDisconnectorSwitches() throws Exception {
        //Lockout line with switches of kind DISCONNECTOR
        createLineAndSwitches(SwitchKind.DISCONNECTOR, false);
        testLockoutLine("line1");
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // line not existing
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("notFound");
        String modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertNull(getNetwork().getLine("notFound"));
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "notFound").getMessage(),
                modificationInfos.getErrorType().name(), reportService);

        // modification action empty
        modificationInfos.setEquipmentId("line2");
        modificationInfos.setAction(null);
        modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().isBadRequest(),
                    content().string(new NetworkModificationException(OPERATING_ACTION_TYPE_EMPTY).getMessage())
            );

        // modification action not existing
        modificationJson = modificationJson.replace("LOCKOUT", "INVALID_ACTION"); // note: should never happen in real
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(
                        status().is4xxClientError());

        // Add a line that can't be disconnected : with fictitious switches
        Line line1 = createLineAndSwitches(SwitchKind.BREAKER, true);
        assertNotNull(line1);
        modificationInfos.setEquipmentId("line1");
        modificationInfos.setAction(OperatingStatusModificationInfos.ActionType.LOCKOUT);
        modificationJson = mapper.writeValueAsString(modificationInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertNull(getNetwork().getLine("line1").getExtension(OperatingStatus.class));
        assertLogMessage(new NetworkModificationException(OPERATING_STATUS_MODIFICATION_ERROR, "Unable to disconnect all equipment ends").getMessage(),
                modificationInfos.getErrorType().name(), reportService);

    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", createdValues.get("energizedVoltageLevelId"));
        assertEquals("LOCKOUT", createdValues.get("action"));
        assertEquals("line2", createdValues.get("equipmentId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("OPERATING_STATUS_MODIFICATION", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("energizedVoltageLevelId", updatedValues.get("energizedVoltageLevelId"));
        assertEquals("SWITCH_ON", updatedValues.get("action"));
        assertEquals("line1", updatedValues.get("equipmentId"));
    }
}
