/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.iidm.network.Network;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.DeleteVoltageLevelOnLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Tag("IntegrationTest")
public class DeleteVoltageLevelOnLineTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkCreation.createForDeleteVoltageLevelOnLine(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteVoltageLevelOnLineInfos.builder()
               .lineToAttachTo1Id("l1")
               .lineToAttachTo2Id("l2")
               .replacingLine1Id("replacementLineId")
               .replacingLine1Name("replacementLine")
               .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("line00")
                .lineToAttachTo2Id("line11")
                .replacingLine1Id("replacingLineId2")
                .replacingLine1Name("replacingLine2")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getVoltageLevel("v1"));
        assertNull(getNetwork().getSubstation("s1"));
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("replacementLineId"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getVoltageLevel("v1"));
        assertNotNull(getNetwork().getSubstation("s1"));
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("replacementLineId"));
    }

    @Test
    public void createWithInvalidLineIdTest() throws Exception {
        // test create with incorrect line id
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = DeleteVoltageLevelOnLineInfos.builder()
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("ll")
                .replacingLine1Id("replacementLineId")
                .build();
        var objectWriter = mapper.writer().withDefaultPrettyPrinter();
        String json = objectWriter.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mockMvc.perform(MockMvcRequestBuilders.post(getNetworkModificationUri()).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "ll").getMessage(),
                deleteVoltageLevelOnLineInfos.getErrorType().name(), reportService);
    }

    @Test
    public void createNewLineWithExistingIdTest() throws Exception {
        // try to create an already existing line
        DeleteVoltageLevelOnLineInfos deleteVoltageLevelOnLineInfos = (DeleteVoltageLevelOnLineInfos) buildModification();
        deleteVoltageLevelOnLineInfos.setReplacingLine1Id("l2");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(deleteVoltageLevelOnLineInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l2").getMessage(),
                deleteVoltageLevelOnLineInfos.getErrorType().name(), reportService);
    }
}
