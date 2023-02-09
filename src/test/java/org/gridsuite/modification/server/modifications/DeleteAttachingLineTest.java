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
import org.gridsuite.modification.server.dto.DeleteAttachingLineInfos;
import org.gridsuite.modification.server.dto.ModificationInfos;
import org.gridsuite.modification.server.utils.MatcherModificationInfos;
import org.gridsuite.modification.server.utils.NetworkWithTeePoint;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationException.Type.DELETE_ATTACHING_LINE_ERROR;
import static org.gridsuite.modification.server.NetworkModificationException.Type.LINE_ALREADY_EXISTS;
import static org.gridsuite.modification.server.utils.MatcherDeleteAttachingLineInfos.createMatcherDeleteAttachingLineInfos;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
public class DeleteAttachingLineTest extends AbstractNetworkModificationTest {

    @Override
    protected Network createNetwork(UUID networkUuid) {
        return NetworkWithTeePoint.create(networkUuid);
    }

    @Override
    protected ModificationInfos buildModification() {
        return DeleteAttachingLineInfos.builder()
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
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l2")
                .attachedLineId("l3")
                .replacingLine1Id("replacingLineIdEdited")
                .replacingLine1Name("replacingLine")
                .build();
    }

    @Override
    protected MatcherModificationInfos createMatcher(ModificationInfos modificationInfos) {
        return createMatcherDeleteAttachingLineInfos((DeleteAttachingLineInfos) modificationInfos);
    }

    @Override
    protected void assertNetworkAfterCreation() {
        assertNull(getNetwork().getLine("l1"));
        assertNull(getNetwork().getLine("l2"));
        assertNotNull(getNetwork().getLine("replacingLineId"));
    }

    @Override
    protected void assertNetworkAfterDeletion() {
        assertNotNull(getNetwork().getLine("l1"));
        assertNotNull(getNetwork().getLine("l2"));
        assertNull(getNetwork().getLine("replacingLineId"));
    }

    @SneakyThrows
    @Test
    public void createWithInvalidLineIdTest() {
        // test create with incorrect line id
        DeleteAttachingLineInfos deleteAttachingLineInfos = DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("ll")
                .attachedLineId("l2")
                .replacingLine1Id("replacementLineId")
                .build();
        var objectWriter = mapper.writer().withDefaultPrettyPrinter();
        String json = objectWriter.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(MockMvcRequestBuilders.post(getNetworkModificationUri()).content(json).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is5xxServerError(),
                    content().string(new NetworkModificationException(DELETE_ATTACHING_LINE_ERROR, "Line ll is not found").getMessage())
            );
    }

    @SneakyThrows
    @Test
    public void createWithNoAttachmentPointTest() {
        DeleteAttachingLineInfos deleteAttachingLineInfos = DeleteAttachingLineInfos.builder()
                .lineToAttachTo1Id("l1")
                .lineToAttachTo2Id("l3")
                .attachedLineId("l1")
                .replacingLine1Id("replacementLineId")
                .build();
        var objectWriter = mapper.writer().withDefaultPrettyPrinter();
        String json = objectWriter.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(MockMvcRequestBuilders.post(getNetworkModificationUri()).content(json).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is5xxServerError(),
                    content().string(new NetworkModificationException(DELETE_ATTACHING_LINE_ERROR, "Unable to find the attachment point and the tapped voltage level from lines l1, l3 and l1").getMessage())
            );
    }

    @SneakyThrows
    @Test
    public void createNewLineWithExistingIdTest() {
        // try to create an already existing line
        DeleteAttachingLineInfos deleteAttachingLineInfos = (DeleteAttachingLineInfos) buildModification();
        deleteAttachingLineInfos.setReplacingLine1Id("l2");
        String lineAttachToAbsentLineJson = mapper.writeValueAsString(deleteAttachingLineInfos);
        mockMvc.perform(post(getNetworkModificationUri()).content(lineAttachToAbsentLineJson).contentType(MediaType.APPLICATION_JSON))
            .andExpectAll(
                    status().is4xxClientError(),
                    content().string(new NetworkModificationException(LINE_ALREADY_EXISTS, "l2").getMessage())
            );
    }
}
