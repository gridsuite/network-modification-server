/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.powsybl.iidm.network.Network;
import lombok.SneakyThrows;
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.junit.Test;
import org.junit.jupiter.api.Tag;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_NOT_FOUND;
import static org.gridsuite.modification.server.utils.TestUtils.assertLogMessage;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Tag("IntegrationTest")
public class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineId")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected ModificationInfos buildModificationUpdate() {
        return DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineIdEdited")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected void assertAfterNetworkModificationCreation() {
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("replacingLineId"));
    }

    @Override
    protected void assertAfterNetworkModificationDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("replacingLineId"));
    }

    @Test
    public void createWithInvalidLineIdTest() throws Exception {
        // test create with incorrect line id
        DeleteAttachingLineInfos deleteAttachingLineInfos = DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("ll")
                .attachedLineId("l2")
                .replacingLine1Id("replacementLineId")
                .build();
        var objectWriter = mapper.writer().withDefaultPrettyPrinter();
        String json = objectWriter.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(MockMvcRequestBuilders.post(getNetworkModificationUri()).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage(new NetworkModificationException(LINE_NOT_FOUND, "ll").getMessage(),
                deleteAttachingLineInfos.getErrorType().name(), reportService);
    }

    @Test
    public void createWithNoAttachmentPointTest() throws Exception {
        DeleteAttachingLineInfos deleteAttachingLineInfos = DeleteAttachingLineInfos.builder()
                .stashed(false)
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l3")
                .attachedLineId("l1")
                .replacingLine1Id("replacementLineId")
                .build();
        var objectWriter = mapper.writer().withDefaultPrettyPrinter();
        String json = objectWriter.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(MockMvcRequestBuilders.post(getNetworkModificationUri()).content(json).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
        assertLogMessage("Unable to find the attachment point and the tapped voltage level from lines l1, l3 and l1",
                deleteAttachingLineInfos.getErrorType().name(), reportService);
    }

    @Test
    public void createNewLineWithExistingIdTest() throws Exception {
        // try to create an already existing line
        DeleteAttachingLineInfos deleteAttachingLineInfos = (DeleteAttachingLineInfos) buildModification();
        deleteAttachingLineInfos.setReplacingLine1Id("l2");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
//        assertLogMessage(new NetworkModificationException(LINE_ALREADY_EXISTS, "l2").getMessage(),
//                deleteAttachingLineInfos.getErrorType().name(), reportService);
    }

    @Override
    @SneakyThrows
    protected void testCreationModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("DELETE_ATTACHING_LINE", modificationInfos.getMessageType());
        Map<String, String> createdValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", createdValues.get("attachedLineId"));
        assertEquals("l1", createdValues.get("lineToAttachTo1Id"));
        assertEquals("l2", createdValues.get("lineToAttachTo2Id"));
    }

    @Override
    @SneakyThrows
    protected void testUpdateModificationMessage(ModificationInfos modificationInfos) {
        assertEquals("DELETE_ATTACHING_LINE", modificationInfos.getMessageType());
        Map<String, String> updatedValues = mapper.readValue(modificationInfos.getMessageValues(), new TypeReference<>() { });
        assertEquals("l3", updatedValues.get("attachedLineId"));
        assertEquals("l1", updatedValues.get("lineToAttachTo1Id"));
        assertEquals("l2", updatedValues.get("lineToAttachTo2Id"));
    }
}
