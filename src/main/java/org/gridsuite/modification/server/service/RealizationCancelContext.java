/**
 * Copyright (c) 2021, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.commons.PowsyblException;
import org.springframework.messaging.Message;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.support.MessageBuilder;

import javax.validation.constraints.NotNull;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public class RealizationCancelContext {

    private final String receiver;

    public RealizationCancelContext(String receiver) {
        this.receiver = receiver;
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

    public static RealizationCancelContext fromMessage(@NotNull Message<String> message) {
        MessageHeaders headers = message.getHeaders();
        String receiver = getNonNullHeader(headers, "receiver");
        return new RealizationCancelContext(receiver);
    }

    public Message<String> toMessage() {
        return MessageBuilder.withPayload("")
                .setHeader("receiver", receiver)
                .build();
    }
}
