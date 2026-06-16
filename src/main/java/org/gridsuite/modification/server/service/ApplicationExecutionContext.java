/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import lombok.AllArgsConstructor;
import lombok.Getter;
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import java.io.UncheckedIOException;
import java.util.List;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NotificationService.RECEIVER_HEADER;

/**
 * @author Ayoub LABIDI <ayoub.labidi_externe at rte-france.com>
 */
@Getter
@AllArgsConstructor
public class ApplicationExecutionContext {

    private static final String GROUP_UUID_HEADER = "groupUuid";
    private static final String MODIFICATION_UUIDS_HEADER = "modificationUuids";

    private final UUID groupUuid;
    private final List<UUID> modificationUuids;
    private final List<ModificationApplicationContext> applicationContexts;
    private final String receiver;

    private static String getNonNullHeader(MessageHeaders headers, String name) {
        String header = (String) headers.get(name);
        if (header == null) {
            throw new PowsyblException("Header '" + name + "' not found");
        }
        return header;
    }

    public static ApplicationExecutionContext fromMessage(Message<String> message, ObjectMapper objectMapper) {
        MessageHeaders headers = message.getHeaders();
        UUID groupUuid = UUID.fromString(getNonNullHeader(headers, GROUP_UUID_HEADER));
        String receiver = getNonNullHeader(headers, RECEIVER_HEADER);
        try {
            List<UUID> modificationUuids = objectMapper.readValue(
                getNonNullHeader(headers, MODIFICATION_UUIDS_HEADER),
                new TypeReference<>() { });
            List<ModificationApplicationContext> applicationContexts = objectMapper.readValue(
                message.getPayload(),
                new TypeReference<>() { });
            return new ApplicationExecutionContext(groupUuid, modificationUuids, applicationContexts, receiver);
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
    }

    public Message<String> toMessage(ObjectMapper objectMapper) {
        try {
            String contextsJson = objectMapper.writeValueAsString(applicationContexts);
            String uuidsJson = objectMapper.writeValueAsString(modificationUuids);
            return MessageBuilder.withPayload(contextsJson)
                .setHeader(GROUP_UUID_HEADER, groupUuid.toString())
                .setHeader(MODIFICATION_UUIDS_HEADER, uuidsJson)
                .setHeader(RECEIVER_HEADER, receiver)
                .build();
        } catch (JsonProcessingException e) {
            throw new UncheckedIOException(e);
        }
    }
}
