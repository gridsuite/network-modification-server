/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import lombok.NonNull;
import org.gridsuite.modification.server.dto.BuildInfos;
import org.gridsuite.modification.server.dto.WorkflowType;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import java.io.UncheckedIOException;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NotificationService.*;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class BuildExecContext {

    private final UUID networkUuid;

    private final BuildInfos buildInfos;

    private final String receiver;

    private final WorkflowType workflowType;

    private final String workflowInfos;

    public BuildExecContext(@NonNull UUID networkUuid, @NonNull BuildInfos buildInfos, @NonNull String receiver) {
        this.networkUuid = networkUuid;
        this.buildInfos = buildInfos;
        this.receiver = receiver;
        this.workflowType = null;
        this.workflowInfos = null;
    }

    public BuildExecContext(@NonNull UUID networkUuid, @NonNull BuildInfos buildInfos, @NonNull String receiver, WorkflowType workflowType, String workflowInfos) {
        this.networkUuid = networkUuid;
        this.buildInfos = buildInfos;
        this.receiver = receiver;
        this.workflowType = workflowType;
        this.workflowInfos = workflowInfos;
    }

    public UUID getNetworkUuid() {
        return networkUuid;
    }

    public BuildInfos getBuildInfos() {
        return buildInfos;
    }

    public String getReceiver() {
        return receiver;
    }

    public WorkflowType getWorkflowType() {
        return workflowType;
    }

    public String getWorkflowInfos() {
        return workflowInfos;
    }

    private static String getNonNullHeader(MessageHeaders headers, String name) {
        String header = (String) headers.get(name);
        if (header == null) {
            throw new PowsyblException("Header '" + name + "' not found");
        }
        return header;
    }

    public static BuildExecContext fromMessage(@NonNull Message<String> message, ObjectMapper objectMapper) {
        MessageHeaders headers = message.getHeaders();
        UUID networkUuid = UUID.fromString(getNonNullHeader(headers, NETWORK_UUID_HEADER));
        String receiver = getNonNullHeader(headers, RECEIVER_HEADER);
        String workflowTypeStr = message.getHeaders().get(WORKFLOW_TYPE_HEADER, String.class);
        WorkflowType workflowType = workflowTypeStr != null ? WorkflowType.valueOf(workflowTypeStr) : null;
        String workflowInfos = headers.get(WORKFLOW_INFOS_HEADER, String.class);
        BuildInfos infos;
        try {
            infos = objectMapper.readValue(message.getPayload(), BuildInfos.class);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return new BuildExecContext(networkUuid, infos, receiver, workflowType, workflowInfos);
    }

    public Message<String> toMessage(ObjectMapper objectMapper) {
        String buildInfosJson;
        try {
            buildInfosJson = objectMapper.writeValueAsString(buildInfos);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return MessageBuilder.withPayload(buildInfosJson)
            .setHeader(NETWORK_UUID_HEADER, networkUuid.toString())
            .setHeader(RECEIVER_HEADER, receiver)
            .setHeader(WORKFLOW_TYPE_HEADER, workflowType)
            .setHeader(WORKFLOW_INFOS_HEADER, workflowInfos)
            .build();
    }
}
