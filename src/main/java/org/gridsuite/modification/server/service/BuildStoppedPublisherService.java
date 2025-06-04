/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.WorkflowType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

import static org.gridsuite.modification.server.service.NotificationService.WORKFLOW_INFOS_HEADER;
import static org.gridsuite.modification.server.service.NotificationService.WORKFLOW_TYPE_HEADER;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Service
public class BuildStoppedPublisherService {

    private static final String CATEGORY_BROKER_OUTPUT = BuildStoppedPublisherService.class.getName() + ".output-broker-messages";

    private static final Logger LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);

    @Autowired
    private StreamBridge stoppedMessagePublisher;

    public void publishCancel(String receiver, String cancelMessage, WorkflowType workflowType, String workflowInfos) {
        publish(receiver, cancelMessage, workflowType, workflowInfos);
    }

    private void publish(String receiver, String stopMessage, WorkflowType workflowType, String workflowInfos) {
        MessageBuilder<String> message = MessageBuilder
                .withPayload("")
                .setHeader("receiver", receiver)
                .setHeader("message", stopMessage);

        if (workflowType != null) {
            message.setHeader(WORKFLOW_TYPE_HEADER, workflowType);
        }

        if (workflowInfos != null) {
            message.setHeader(WORKFLOW_INFOS_HEADER, workflowInfos);
        }

        LOGGER.debug("Sending message : {}", message.build());
        stoppedMessagePublisher.send("publishStoppedBuild-out-0", message.build());
    }
}
