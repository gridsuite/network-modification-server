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
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.BUSBAR_SECTION_NOT_FOUND;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
public class LineSplitWithVoltageLevelTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineSplitWithVoltageLevelInfos.builder()
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
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
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
    public void testCreateWithExistingLines() throws Exception {
        // try to create an already existing line
        LineSplitWithVoltageLevelInfos tryWithNewLine1Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        String tryWithNewLine1IdJson = mapper.writeValueAsString(tryWithNewLine1Id);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine1IdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                tryWithNewLine1Id.getErrorType().name(), reportService);

        // same test with "newLine2Id"
        LineSplitWithVoltageLevelInfos tryWithNewLine2Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine2Id("line1");
        String tryWithNewLine2IdJson = mapper.writeValueAsString(tryWithNewLine2Id);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine2IdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage(),
                tryWithNewLine2Id.getErrorType().name(), reportService);
    }

    @Test
    public void testCreateWithWrongBusBar() throws Exception {
        // not existing busbar
        LineSplitWithVoltageLevelInfos tryWithBadId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithBadId.setBbsOrBusId("999A");
        String tryWithBadIdJson = mapper.writeValueAsString(tryWithBadId);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithBadIdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "999A").getMessage(),
                tryWithBadId.getErrorType().name(), reportService);

        // try with a switch, not a busbar
        LineSplitWithVoltageLevelInfos tryWithSwitchId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithSwitchId.setBbsOrBusId("v1d1");
        String tryWithSwitchIdJson = mapper.writeValueAsString(tryWithSwitchId);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithSwitchIdJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "v1d1").getMessage(),
                tryWithBadId.getErrorType().name(), reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_SPLIT_WITH_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2", createdValues.get("lineToSplitId"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("LINE_SPLIT_WITH_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2Edited", updatedValues.get("lineToSplitId"));
    }
}
