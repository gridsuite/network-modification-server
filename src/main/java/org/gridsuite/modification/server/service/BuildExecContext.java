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
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import java.io.UncheckedIOException;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class BuildExecContext {

    private final UUID networkUuid;

    private final BuildInfos buildInfos;

    private final String receiver;

    public BuildExecContext(@NonNull UUID networkUuid, @NonNull BuildInfos buildInfos, @NonNull String receiver) {
        this.networkUuid = networkUuid;
        this.buildInfos = buildInfos;
        this.receiver = receiver;
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

    private static String getNonNullHeader(MessageHeaders headers, String name) {
        String header = (String) headers.get(name);
        if (header == null) {
            throw new PowsyblException("Header '" + name + "' not found");
        }
        return header;
    }

    public static BuildExecContext fromMessage(@NonNull Message<String> message, ObjectMapper objectMapper) {
        MessageHeaders headers = message.getHeaders();
        UUID networkUuid = UUID.fromString(getNonNullHeader(headers, "networkUuid"));
        String receiver = getNonNullHeader(headers, "receiver");
        BuildInfos infos;
        try {
            infos = objectMapper.readValue(message.getPayload(), BuildInfos.class);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return new BuildExecContext(networkUuid, infos, receiver);
    }

    public Message<String> toMessage(ObjectMapper objectMapper) {
        String buildInfosJson;
        try {
            buildInfosJson = objectMapper.writeValueAsString(buildInfos);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return MessageBuilder.withPayload(buildInfosJson)
            .setHeader("networkUuid", networkUuid.toString())
            .setHeader("receiver", receiver)
            .build();
    }
}
