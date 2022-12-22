/*
  Copyright (c) 2022, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.ModificationType;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.springframework.http.MediaType;

import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ATTACH_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.MatcherLineAttachToVoltageLevelInfos.createMatcherLineAttachToVoltageLevelInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author David Braquart <david.braquart at rte-france.com>
 */
public class LineAttachToVoltageLevelTest extends AbstractNetworkModificationTest {

    private LineCreationInfos getAttachmentLine(String lineName) {
        return LineCreationInfos.builder()
                .type(ModificationType.LINE_CREATION)
                .equipmentId(lineName)
                .seriesResistance(50.6)
                .seriesReactance(25.3)
                .build();
    }

    private VoltageLevelCreationInfos getNewVoltageLevel() {
        return VoltageLevelCreationInfos.builder()
                .type(ModificationType.VOLTAGE_LEVEL_CREATION)
                .equipmentId("newVlName")
                .equipmentName("NewVoltageLevel")
                .nominalVoltage(379.3)
                .substationId("s1")
                .busbarSections(List.of(new BusbarSectionCreationInfos("v1bbs", "BBS1", 1, 1)))
                .busbarConnections(List.of())
                .build();
    }

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.create(networkUuid, true);
    }

    @Override
    protected ModificationInfos buildModification() {
        return LineAttachToVoltageLevelInfos.builder()
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
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
                .type(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL)
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
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherLineAttachToVoltageLevelInfos((LineAttachToVoltageLevelInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        // new equipments in the network:
        assertNotNull(getNetwork().getLine("attachmentLine"));
        assertNotNull(getNetwork().getLine("nl1"));
        assertNotNull(getNetwork().getLine("nl2"));
        assertNotNull(getNetwork().getVoltageLevel("AttPointId"));
        // replaced line is gone
        assertNull(getNetwork().getLine("line3"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNull(getNetwork().getLine("attachmentLine"));
        assertNull(getNetwork().getLine("nl1"));
        assertNull(getNetwork().getLine("nl2"));
        assertNull(getNetwork().getVoltageLevel("AttPointId"));
        assertNotNull(getNetwork().getLine("line3"));
    }

    @SneakyThrows
    @Test
    public void testCreateWithErrors() {
        LineAttachToVoltageLevelInfos lineAttachToAbsentLine = (LineAttachToVoltageLevelInfos) buildModification();
        lineAttachToAbsentLine.setLineToAttachToId("absent_line_id");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(lineAttachToAbsentLine);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(LINE_NOT_FOUND, "absent_line_id").getMessage())
            );
        //testNetworkModificationsCount(TEST_GROUP_ID, 1);

        LineAttachToVoltageLevelInfos lineMissingLine = (LineAttachToVoltageLevelInfos) buildModification();
        lineMissingLine.setAttachmentLine(null); // we omit a mandatory input data
        String lineMissingLineJson = mapper.writeValueAsString(lineMissingLine);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineMissingLineJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is5xxServerError(),
                    content().string(new NetworkModificationException(LINE_ATTACH_ERROR, "Missing required attachment line description").getMessage())
            );
        //testNetworkModificationsCount(TEST_GROUP_ID, 2);
    }
}
