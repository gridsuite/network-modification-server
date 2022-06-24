package org.gridsuite.modification.server.service;

import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import com.powsybl.commons.PowsyblException;

import lombok.NonNull;

public class BuildFailedContext {
    private final String receiver;
    private final String errorMessage;

    public BuildFailedContext(String receiver, String errorMessage) {
        this.receiver = receiver;
        this.errorMessage = errorMessage;
    }

    public String getReceiver() {
        return receiver;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    private static String getNonNullHeader(MessageHeaders headers, String name) {
        String header = (String) headers.get(name);
        if (header == null) {
            throw new PowsyblException("Header '" + name + "' not found");
        }
        return header;
    }

    public static BuildFailedContext fromMessage(@NonNull Message<String> message) {
        MessageHeaders headers = message.getHeaders();
        String receiver = getNonNullHeader(headers, "receiver");
        String errMessage = getNonNullHeader(headers, "message");
        return new BuildFailedContext(receiver, errMessage);
    }

    public Message<String> toMessage() {
        return MessageBuilder.withPayload("")
                .setHeader("receiver", receiver)
                .build();
    }
}
