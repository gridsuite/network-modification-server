/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.springframework.context.annotation.Bean;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;

import java.util.function.Consumer;

/**
 * @author Ayoub LABIDI <ayoub.labidi_externe at rte-france.com>
 */
@Service
@RequiredArgsConstructor
public class ModificationApplicationWorkerService {

    @NonNull private final NetworkModificationService networkModificationService;
    @NonNull private final NotificationService notificationService;
    @NonNull private final ObjectMapper objectMapper;

    @Bean
    public Consumer<Message<String>> consumeApplication() {
        return message -> {
            ApplicationExecContext ctx = ApplicationExecContext.fromMessage(message, objectMapper);
            notificationService.emitApplicationResultMessage(
                new NetworkModificationsResult(
                    ctx.getModificationUuids(),
                    networkModificationService.applyModificationsByUuids(
                        ctx.getGroupUuid(), ctx.getModificationUuids(), ctx.getApplicationContexts())),
                ctx.getReceiver());
        };
    }
}
