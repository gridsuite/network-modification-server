/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LinesAttachToSplitLinesInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.springframework.http.MediaType;

import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.report.NetworkModificationServerReportResourceBundle.ERROR_MESSAGE_KEY;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.jupiter.api.Assertions.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Tag("IntegrationTest")
class LinesAttachToSplitLinesTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LinesAttachToSplitLinesInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .voltageLevelId("v4")
                .bbsBusId("bbs4")
                .replacingLine1Id("nl1")
                .replacingLine1Name("NewLine1")
                .replacingLine2Id("nl2")
                .replacingLine2Name("NewLine2")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LinesAttachToSplitLinesInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("newline1")
                .lineToAttachTo2Id("newline2")
                .attachedLineId("newline3")
                .voltageLevelId("newv4")
                .bbsBusId("new1.A")
                .replacingLine1Id("newnl4")
                .replacingLine1Name("newNewLine4")
                .replacingLine2Id("newnl5")
                .replacingLine2Name("newNewLine5")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        // 3 lines are gone
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("l3"));
        // v2 is gone
        assertNull(getNetwork().getVoltageLevel("v2"));
        assertEquals(3, getNetwork().getVoltageLevelCount());
        // new lines:
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("l3"));
        assertNotNull(getNetwork().getVoltageLevel("v2"));
        assertEquals(4, getNetwork().getVoltageLevelCount());
        assertNull(getNetwork().getLine("nl1"));
        assertNull(getNetwork().getLine("nl2"));
    }

    @Test
    void testCreateWithErrors() throws Exception {
        // use an unexisting line
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setLineToAttachTo1Id("absent_line_id");
        String lineAttachToAbsentLineJson = getJsonBody(linesAttachToSplitLinesInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
        // try to create an already existing line
        linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setReplacingLine1Id("l1");
        lineAttachToAbsentLineJson = getJsonBody(linesAttachToSplitLinesInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
        // same test on 'replacingLine2Id'
        linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setReplacingLine2Id("l1");
        lineAttachToAbsentLineJson = getJsonBody(linesAttachToSplitLinesInfos, null);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                ERROR_MESSAGE_KEY, reportService);
    }

    @Override
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINES_ATTACH_TO_SPLIT_LINES", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", createdValues.get("attachedLineId"));
    }

    @Override
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) throws Exception {
        assertEquals("LINES_ATTACH_TO_SPLIT_LINES", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("newline3", updatedValues.get("attachedLineId"));
    }
}
