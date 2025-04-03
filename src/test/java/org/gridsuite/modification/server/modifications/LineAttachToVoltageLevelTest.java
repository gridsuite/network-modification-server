/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.*;
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
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Tag("IntegrationTest")
class LineAttachToVoltageLevelTest extends AbstractNetworkModificationTest {
    private static LineCreationInfos getAttachmentLine(String lineName) {
        return LineCreationInfos.builder()
                .stashed(false)
                .equipmentId(lineName)
                .r(50.6)
                .x(25.3)
                .build();
    }

    private static VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
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
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineAttachToVoltageLevelInfos.builder()
                .stashed(false)
                .lineToAttachToId("line3")
                .percent(10.0)
                .attachmentPointId("AttPointId")   // created VL
                .attachmentPointName("attPointName")
                .mayNewVoltageLevelInfos(null)
                .existingVoltageLevelId("v4")     // use existing VL
                .bbsOrBusId("1.A")
                .attachmentLine(getAttachmentLine("attachmentLine"))   // created Line
                .newLine1Id("nl1")
                .newLine1Name("NewLine1")
                .newLine2Id("nl2")
                .newLine2Name("NewLine2")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineAttachToVoltageLevelInfos.builder()
                .stashed(false)
                .lineToAttachToId("line2")
                .percent(30.0)
                .attachmentPointId("newAttPointId")
                .attachmentPointName("newAttPointName")
                .mayNewVoltageLevelInfos(getNewVoltageLevel())
                .existingVoltageLevelId(null)
                .bbsOrBusId("2.A")
                .attachmentLine(getAttachmentLine("newLineName"))
                .newLine1Id("newLine1Id")
                .newLine1Name("newLine1Name")
                .newLine2Id("newLine2Id")
                .newLine2Name("newLine2Name")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        // new equipments in the network:
        assertNotNull(getNetwork().getLine("attachmentLine"));
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
        assertNotNull(getNetwork().getVoltageLevel("AttPointId"));
        // replaced line is gone
        assertNull(getNetwork().getLine("line3"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNull(getNetwork().getLine("attachmentLine"));
        assertNull(getNetwork().getLine("nl1"));
        assertNull(getNetwork().getLine("nl2"));
        assertNull(getNetwork().getVoltageLevel("AttPointId"));
        assertNotNull(getNetwork().getLine("line3"));
    }

    private void tryToCreateLineWithExistingId(LineAttachToVoltageLevelInfos tryWithExistingLine, String existingLineId) throws Exception {
        String tryWithExistingLineJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(tryWithExistingLine, List.of(buildApplicationContext())));
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithExistingLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, existingLineId).getMessage(),
                tryWithExistingLine.getErrorType().name(), reportService);
    }

    @Test
    void testCreateWithErrors() throws Exception {
        LineAttachToVoltageLevelInfos lineAttachToAbsentLine = (LineAttachToVoltageLevelInfos) buildModification();
        lineAttachToAbsentLine.setLineToAttachToId("absent_line_id");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(org.springframework.data.util.Pair.of(lineAttachToAbsentLine, List.of(buildApplicationContext())));
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage(),
                lineAttachToAbsentLine.getErrorType().name(), reportService);
        testNetworkModificationsCount(getGroupId(), 1);

        LineAttachToVoltageLevelInfos lineMissingLine = (LineAttachToVoltageLevelInfos) buildModification();
        lineMissingLine.setAttachmentLine(null); // we omit a mandatory input data
        String lineMissingLineJson = mapper.writeValueAsString(lineMissingLine);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineMissingLineJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(LINE_ATTACH_DESCRIPTION_ERROR, "Missing required attachment line description").getMessage())
            );
        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Test
    void testCreateWithExistingEquipments() throws Exception {
        // try to create an already existing line
        LineAttachToVoltageLevelInfos tryWithNewLine1Id = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        tryToCreateLineWithExistingId(tryWithNewLine1Id, "line1");
        // same test with "newLine2Id"
        LineAttachToVoltageLevelInfos tryWithNewLine2Id = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine1Id("line3");
        tryToCreateLineWithExistingId(tryWithNewLine2Id, "line3");
        // same test with "attachmentLine"
        LineAttachToVoltageLevelInfos tryWithEquipmentId = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithEquipmentId.setAttachmentLine(getAttachmentLine("line2"));
        tryToCreateLineWithExistingId(tryWithEquipmentId, "line2");
        // try to create an already existing VL
        LineAttachToVoltageLevelInfos tryWithAttachmentPointId = (LineAttachToVoltageLevelInfos) buildModification();
        tryWithAttachmentPointId.setAttachmentPointId("v5");
        String tryWithExistingLineJson = mapper.writeValueAsString(tryWithAttachmentPointId);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithExistingLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(VOLTAGE_LEVEL_ALREADY_EXISTS, "v5").getMessage(),
                tryWithAttachmentPointId.getErrorType().name(), reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_ATTACH_TO_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line3", createdValues.get("lineToAttachToId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINE_ATTACH_TO_VOLTAGE_LEVEL", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("line2", updatedValues.get("lineToAttachToId"));
    }
}
