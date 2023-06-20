/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.LinesAttachToSplitLinesInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
@Tag("IntegrationTest")
public class LinesAttachToSplitLinesTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LinesAttachToSplitLinesInfos.builder()
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
    protected void assertNetworkAfterCreation() {
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
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("l3"));
        assertNotNull(getNetwork().getVoltageLevel("v2"));
        assertEquals(4, getNetwork().getVoltageLevelCount());
        assertNull(getNetwork().getLine("nl1"));
        assertNull(getNetwork().getLine("nl2"));
    }

    @Test
    public void testCreateWithErrors() throws Exception {
        // use an unexisting line
        LinesAttachToSplitLinesInfos linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setLineToAttachTo1Id("absent_line_id");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(linesAttachToSplitLinesInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage(),
                linesAttachToSplitLinesInfos.getErrorType().name(), reportService);
        // try to create an already existing line
        linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setReplacingLine1Id("l1");
        lineAttachToAbsentLineJson = mapper.writeValueAsString(linesAttachToSplitLinesInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                linesAttachToSplitLinesInfos.getErrorType().name(), reportService);
        // same test on 'replacingLine2Id'
        linesAttachToSplitLinesInfos = (LinesAttachToSplitLinesInfos) buildModification();
        linesAttachToSplitLinesInfos.setReplacingLine2Id("l1");
        lineAttachToAbsentLineJson = mapper.writeValueAsString(linesAttachToSplitLinesInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l1").getMessage(),
                linesAttachToSplitLinesInfos.getErrorType().name(), reportService);
    }
}
