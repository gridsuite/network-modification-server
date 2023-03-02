/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.*;
import static org.gridsuite.modification.server.utils.MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

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
            .equipmentId("vl1")
            .equipmentName("NewVoltageLevel")
            .nominalVoltage(379.3)
            .substationId("s1")
            .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
            .busbarConnections(List.of())
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
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherLineSplitWithVoltageLevelInfos((LineSplitWithVoltageLevelInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLine("line2"));
        assertNotNull(getNetwork().getLine("nl1v"));
        assertNotNull(getNetwork().getLine("nl2v"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getLine("nl1v"));
        assertNull(getNetwork().getLine("nl2v"));
    }

    @SneakyThrows
    @Test
    public void testCreateWithExistingLines() {
        // try to create an already existing line
        LineSplitWithVoltageLevelInfos tryWithNewLine1Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine1Id.setNewLine1Id("line1");
        String tryWithNewLine1IdJson = mapper.writeValueAsString(tryWithNewLine1Id);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine1IdJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage())
            );
        // same test with "newLine2Id"
        LineSplitWithVoltageLevelInfos tryWithNewLine2Id = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithNewLine2Id.setNewLine2Id("line1");
        String tryWithNewLine2IdJson = mapper.writeValueAsString(tryWithNewLine2Id);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithNewLine2IdJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(LINE_ALREADY_EXISTS, "line1").getMessage())
            );
    }

    @SneakyThrows
    @Test
    public void testCreateWithWrongBusBar() {
        // not existing busbar
        LineSplitWithVoltageLevelInfos tryWithBadId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithBadId.setBbsOrBusId("999A");
        String tryWithBadIdJson = mapper.writeValueAsString(tryWithBadId);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithBadIdJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "999A").getMessage())
            );
        // try with a switch, not a busbar
        LineSplitWithVoltageLevelInfos tryWithSwitchId = (LineSplitWithVoltageLevelInfos) buildModification();
        tryWithSwitchId.setBbsOrBusId("v1d1");
        String tryWithSwitchIdJson = mapper.writeValueAsString(tryWithSwitchId);
        mockMvc.perform(post(getNetworkModificationUri()).content(tryWithSwitchIdJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(BUSBAR_SECTION_NOT_FOUND, "v1d1").getMessage())
            );
    }
}
