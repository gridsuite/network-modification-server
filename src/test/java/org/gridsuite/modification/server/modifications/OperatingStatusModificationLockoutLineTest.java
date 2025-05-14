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

import com.powsybl.iidm.network.extensions.OperatingStatus;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperatingStatusModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Map;
import java.util.UUID;

import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.FORCED_OUTAGE;
import static com.powsybl.iidm.network.extensions.OperatingStatus.Status.PLANNED_OUTAGE;
import static org.gridsuite.modification.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
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
        createSwitch(getNetwork().getVoltageLevel("vl1"), "br12", "br12", switchKind, false, false, isFictitious, 0, 3);
        createSwitch(getNetwork().getVoltageLevel("vl2"), "br22", "br22", switchKind, false, false, isFictitious, 0, 3);

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
        String modificationJson = getJsonBody(modificationInfos, null);
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
    void testWithLoadBreaker() throws Exception {
        //Lockout line with switches of kind LOAD_BREAK_SWITCH
        createLineAndSwitches(SwitchKind.LOAD_BREAK_SWITCH, false);
        testLockoutLine("line1");
    }

    @Test
    void testWithFictitiousLoadBreaker() throws Exception {
        //Lockout line with fictitious switches of kind LOAD_BREAK_SWITCH
        createLineAndSwitches(SwitchKind.LOAD_BREAK_SWITCH, true);
        testLockoutLine("line1");
    }

    @Test
    void testWithBreaker() throws Exception {
        //Lockout line with switches of kind BREAKER
        createLineAndSwitches(SwitchKind.BREAKER, false);
        testLockoutLine("line1");
    }

    @Test
    void testWithFictitiousBreaker() throws Exception {
        //Lockout line with fictitious switches of kind BREAKER
        createLineAndSwitches(SwitchKind.BREAKER, true);
        testLockoutLine("line1");
    }

    @Test
    void testWithDisconnector() throws Exception {
        //Lockout line with switches of kind DISCONNECTOR
        createLineAndSwitches(SwitchKind.DISCONNECTOR, false);
        testLockoutLine("line1");
    }

    @Test
    void testWithFictitiousDisconnector() throws Exception {
        //Lockout line with fictitious switches of kind DISCONNECTOR
        createLineAndSwitches(SwitchKind.DISCONNECTOR, true);
        testLockoutLine("line1");
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // line not existing
        OperatingStatusModificationInfos modificationInfos = (OperatingStatusModificationInfos) buildModification();
        modificationInfos.setEquipmentId("notFound");
        String modificationJson = getJsonBody(modificationInfos, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
        assertNull(getNetwork().getLine("notFound"));
        assertLogMessage(new NetworkModificationException(EQUIPMENT_NOT_FOUND, "notFound").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        // modification action empty
        modificationInfos.setEquipmentId("line2");
        modificationInfos.setAction(null);
        modificationJson = getJsonBody(modificationInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().isBadRequest(),
                    content().string(new NetworkModificationException(OPERATING_ACTION_TYPE_EMPTY).getMessage())
            );
        // modification action not existing
        // note: should never happen in real
        mockMvc.perform(post(getNetworkModificationUri()).content(modificationJson.replace("LOCKOUT", "INVALID_ACTION")).contentType(MediaType.APPLICATION_JSON))
                .andExpect(
                        status().is4xxClientError());
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
