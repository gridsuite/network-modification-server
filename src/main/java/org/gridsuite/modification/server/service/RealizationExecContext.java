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
import org.gridsuite.modification.server.dto.RealizationInfos;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import javax.validation.constraints.NotNull;
import java.io.UncheckedIOException;
import java.util.UUID;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class RealizationExecContext {

    private UUID networkUuid;

    private RealizationInfos realizationInfos;

    private String receiver;

    public RealizationExecContext(@NotNull UUID networkUuid, @NotNull RealizationInfos realizationInfos, @NotNull String receiver) {
        this.networkUuid = networkUuid;
        this.realizationInfos = realizationInfos;
        this.receiver = receiver;
    }

    public UUID getNetworkUuid() {
        return networkUuid;
    }

    public RealizationInfos getRealizationInfos() {
        return realizationInfos;
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

    public static RealizationExecContext fromMessage(@NotNull Message<String> message, ObjectMapper objectMapper) {
        MessageHeaders headers = message.getHeaders();
        UUID networkUuid = UUID.fromString(getNonNullHeader(headers, "networkUuid"));
        String receiver = getNonNullHeader(headers, "receiver");
        RealizationInfos infos;
        try {
            infos = objectMapper.readValue(message.getPayload(), RealizationInfos.class);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return new RealizationExecContext(networkUuid, infos, receiver);
    }

    public Message<String> toMessage(ObjectMapper objectMapper) {
        String realizationInfosJson;
        try {
            realizationInfosJson = objectMapper.writeValueAsString(realizationInfos);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
        return MessageBuilder.withPayload(realizationInfosJson)
            .setHeader("networkUuid", networkUuid.toString())
            .setHeader("receiver", receiver)
            .build();
    }
}
