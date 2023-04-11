/**
 * Copyright (c) 2022, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import lombok.NonNull;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

/**
 * @author Seddik Yengui <seddik.yengui at rte-france.com
 */

// Today we don't send notification inside @Transactional block. If this behavior change, we should use @PostCompletion to
// make sure that the notification is sent only when all the work inside @Transactional block is done.
@Service
public class NotificationService {

    private static final String CATEGORY_BROKER_OUTPUT = BuildWorkerService.class.getName() + ".output-broker-messages";
    private static final Logger OUTPUT_MESSAGE_LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);
    public static final String RECEIVER_HEADER = "receiver";
    public static final String NETWORK_UUID_HEADER = "networkUuid";

    @Autowired
    private StreamBridge publisher;

    private void sendMessage(Message<? extends Object> message, String bindingName) {
        OUTPUT_MESSAGE_LOGGER.debug("Sending message : {}", message);
        publisher.send(bindingName, message);
    }

    public void emitBuildResultMessage(@NonNull NetworkModificationResult payload, @NonNull String receiver) {
        Message<NetworkModificationResult> message = MessageBuilder.withPayload(payload)
                .setHeader(RECEIVER_HEADER, receiver)
                .build();
        sendMessage(message, "publishResultBuild-out-0");
    }

    public void emitBuildMessage(@NonNull Message<String> message) {
        sendMessage(message, "publishBuild-out-0");
    }

    public void emitCancelBuildMessage(@NonNull String receiver) {
        Message<String> message = MessageBuilder.withPayload("")
                .setHeader(RECEIVER_HEADER, receiver)
                .build();
        sendMessage(message, "publishCancelBuild-out-0");
    }
}
