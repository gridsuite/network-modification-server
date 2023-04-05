/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.SwitchKind;

import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.CouplingDeviceInfos;
import org.gridsuite.modification.server.dto.LineSplitWithVoltageLevelInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.VoltageLevelCreationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.Arrays;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherLineSplitWithVoltageLevelInfos.createMatcherLineSplitWithVoltageLevelInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class LineSplitWithNewVoltageLevelTest extends AbstractNetworkModificationTest {

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        LineSplitWithVoltageLevelInfos lineSplitAbsentLine = (LineSplitWithVoltageLevelInfos) buildModification();
        lineSplitAbsentLine.setLineToSplitId("absent_line_id");
        String lineSplitAbsentLineJson = mapper.writeValueAsString(lineSplitAbsentLine);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineSplitAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                status().is4xxClientError(),
                content().string(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage())
            );
        testNetworkModificationsCount(getGroupId(), 1);
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        VoltageLevelCreationInfos vl1 = VoltageLevelCreationInfos.builder()
                .equipmentId("newVoltageLevel")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .nominalVoltage(379.1)
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
            .lineToSplitId("line2")
            .percent(10.0)
            .mayNewVoltageLevelInfos(vl1)
            .existingVoltageLevelId(null)
            .bbsOrBusId("newVoltageLevel_1_1")
            .newLine1Id("nl1v")
            .newLine1Name("NewLine1")
            .newLine2Id("nl2v")
            .newLine2Name("NewLine2")
            .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return LineSplitWithVoltageLevelInfos.builder()
            .lineToSplitId("line2Edited")
            .percent(20.0)
            .mayNewVoltageLevelInfos(null)
            .existingVoltageLevelId("v4")
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
        assertNotNull(getNetwork().getVoltageLevel("newVoltageLevel"));
        assertNotNull(getNetwork().getLine("nl1v"));
        assertNotNull(getNetwork().getLine("nl2v"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("line2"));
        assertNull(getNetwork().getVoltageLevel("newVoltageLevel"));
        assertNull(getNetwork().getLine("nl1v"));
        assertNull(getNetwork().getLine("nl2v"));
    }
}
