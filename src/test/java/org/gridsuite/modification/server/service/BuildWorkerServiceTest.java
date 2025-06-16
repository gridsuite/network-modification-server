/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.gridsuite.modification.server.BuildException;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.WorkflowType;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NotificationService.*;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.*;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class BuildWorkerServiceTest {

    @Autowired
    private BuildWorkerService buildWorkerService;
    @Autowired
    private ObjectMapper objectMapper;
    @MockBean
    private NetworkModificationService networkModificationService;
    @MockBean
    private NotificationService notificationService;

    @Test
    void testConsumeBuildWithMalformedInput() {
        assertThrows(
            BuildException.class,
            () -> buildWorkerService.consumeBuild().accept(MessageBuilder.withPayload("wrong message").build()),
            "Failed to read build message");
    }

    @Test
    void testConsumeBuildWithWorkflowInfos() throws JsonProcessingException {
        UUID networkUuid = UUID.randomUUID();
        String receiver = "receiver";
        WorkflowType workflowType = WorkflowType.RERUN_LOAD_FLOW;
        String workflowInfos = "workflowInfos";
        NetworkModificationResult modificationResult = NetworkModificationResult.builder()
            .applicationStatus(NetworkModificationResult.ApplicationStatus.ALL_OK)
            .networkImpacts(List.of())
            .build();
        BuildInfos buildInfos = BuildInfos.builder()
            .originVariantId("origin")
            .destinationVariantId("destination")
            .build();
        Map<String, Object> headers = new HashMap<>();
        headers.put(NETWORK_UUID_HEADER, networkUuid.toString());
        headers.put(RECEIVER_HEADER, receiver);
        headers.put(WORKFLOW_TYPE_HEADER, workflowType.name());
        headers.put(WORKFLOW_INFOS_HEADER, workflowInfos);
        MessageHeaders messageHeaders = new MessageHeaders(headers);

        when(networkModificationService.buildVariant(eq(networkUuid), any(BuildInfos.class))).thenReturn(modificationResult);
        buildWorkerService.consumeBuild().accept(MessageBuilder.createMessage(objectMapper.writeValueAsString(buildInfos), messageHeaders));
        ArgumentCaptor<BuildInfos> buildInfosArgumentCaptor = ArgumentCaptor.forClass(BuildInfos.class);
        verify(networkModificationService, times(1)).buildVariant(eq(networkUuid), buildInfosArgumentCaptor.capture());
        assertThat(buildInfosArgumentCaptor.getValue()).usingRecursiveComparison().isEqualTo(buildInfos);
        verify(notificationService, times(1)).emitBuildResultMessage(modificationResult, receiver, workflowType, workflowInfos);
    }
}
