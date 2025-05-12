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
import org.gridsuite.modification.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
class LineSplitWithVoltageLevelTest extends AbstractNetworkModificationTest {
    private static final String ERROR_MESSAGE_KEY = "network.modification.server.errorMessage";

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineSplitWithVoltageLevelInfos.builder()
            .stashed(false)
            .lineToSplitId("line2")
            .percent(10.0)
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("v4")
            .bbsOrBusId("1.A")
            .newLine1Id("nl1v")
            .newLine1Name("NewLine1")
            .newLine2Id("nl2v")
            .newLine2Name("NewLine2")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        VoltageLevelCreationInfos vl1 = VoltageLevelCreationInfos.builder()
                .stashed(false)
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalV(379.3)
                .substationId("s1")
                .lowVoltageLimit(0.0)
                .highVoltageLimit(10.0)
                .ipMin(0.0)
                .ipMax(10.0)
                .busbarCount(2)
                .sectionCount(2)
                .switchKinds(Arrays.asList(SwitchKind.BREAKER))
                .couplingDevices(Arrays.asList(CouplingDeviceInfos.builder().busbarSectionId1("bbs.nw").busbarSectionId2("bbs.ne").build()))
                .build();

        return LineSplitWithVoltageLevelInfos.builder()
            .stashed(false)
            .lineToSplitId("line2Edited")
            .percent(20.0)
            .mayNewVoltageLevelInfos(vl1)
            .existingVoltageLevelId(null)
            .bbsOrBusId("v1bbsEdited")
            .newLine1Id("nl1vEdited")
            .newLine1Name("NewLine1Edited")
            .newLine2Id("nl2vEdited")
            .newLine2Name("NewLine2Edited")
            .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLine("line2"));
        assertNotNull(getNetwork().getLine("nl1v"));
        assertNotNull(getNetwork().getLine("nl2v"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getLine("nl1v"));
        assertNull(getNetwork().getLine("nl2v"));
    }

    @Test
    void testCreateWithExistingLines() throws Exception {
        // try to create an already existing line
        LineSplitWithVoltageLevelInfos tryWithNewLine1Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        String tryWithNewLine1IdJson = getJsonBody(tryWithNewLine1Id, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine1IdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        // same test with "newLine2Id"
        LineSplitWithVoltageLevelInfos tryWithNewLine2Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine2Id("line1");
        String tryWithNewLine2IdJson = getJsonBody(tryWithNewLine2Id, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine2IdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Test
    void testCreateWithWrongBusBar() throws Exception {
        // not existing busbar
        LineSplitWithVoltageLevelInfos tryWithBadId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithBadId.setBbsOrBusId("999A");
        String tryWithBadIdJson = getJsonBody(tryWithBadId, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithBadIdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "999A").getMessage(),
                ERROR_MESSAGE_KEY, reportService);

        // try with a switch, not a busbar
        LineSplitWithVoltageLevelInfos tryWithSwitchId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithSwitchId.setBbsOrBusId("v1d1");
        String tryWithSwitchIdJson = getJsonBody(tryWithSwitchId, null);

        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithSwitchIdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "v1d1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_SPLIT_WITH_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2", createdValues.get("lineToSplitId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_SPLIT_WITH_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2Edited", updatedValues.get("lineToSplitId"));
    }
}
