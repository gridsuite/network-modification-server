package org.gridsuite.modification.server.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

@Service
public class BuildFailedPublisherService {
    public static final String FAIL_MESSAGE = "Build has failed";

    private static final String CATEGORY_BROKER_OUTPUT = BuildFailedPublisherService.class.getName() + ".output-broker-messages";

    private static final Logger LOGGER = LoggerFactory.getLogger(CATEGORY_BROKER_OUTPUT);

    @Autowired
    private StreamBridge failedMessagePublisher;

    public void publishFail(String receiver, String causeMessage) {
        publish(receiver, FAIL_MESSAGE + " : " + causeMessage);
    }

    private void publish(String receiver, String failMessage) {
        Message<String> message = MessageBuilder
                .withPayload("")
                .setHeader("receiver", receiver)
                .setHeader("message", failMessage)
                .build();
        LOGGER.debug("Sending message : {}", message);
        failedMessagePublisher.send("publishFailedBuild-out-0", message);
    }
}
