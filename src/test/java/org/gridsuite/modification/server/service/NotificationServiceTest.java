/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.ElementReferenceAction;
import org.gridsuite.modification.server.dto.ModificationReferenceData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.messaging.Message;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * @author Sylvain Bouzols <sylvain.bouzols at rte-france.com>
 */
class NotificationServiceTest {
    private NotificationService notificationService;
    private StreamBridge publisher;

    @BeforeEach
    void setUp() {
        publisher = mock(StreamBridge.class);
        notificationService = new NotificationService();
        // Use reflection to inject the mock
        try {
            var field = NotificationService.class.getDeclaredField("publisher");
            field.setAccessible(true);
            field.set(notificationService, publisher);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    void emitModificationReferencesChanged() {
        UUID groupUuid = UUID.randomUUID();
        String userId = "test-user";
        List<ModificationReferenceData> references = List.of(mock(ModificationReferenceData.class));

        notificationService.emitModificationReferencesChanged(ElementReferenceAction.CREATE, groupUuid, references, userId);

        ArgumentCaptor<Message<List<ModificationReferenceData>>> messageCaptor = ArgumentCaptor.forClass(Message.class);
        verify(publisher).send(eq("publishCompositeReference-out-0"), messageCaptor.capture());
        Message<List<ModificationReferenceData>> sentMessage = messageCaptor.getValue();
        assertEquals(references, sentMessage.getPayload());
        assertEquals(ElementReferenceAction.CREATE, sentMessage.getHeaders().get(NotificationService.HEADER_ACTION));
        assertEquals(groupUuid, sentMessage.getHeaders().get(NotificationService.HEADER_GROUP_UUID));
        assertEquals(userId, sentMessage.getHeaders().get(NotificationService.HEADER_USER_ID));
    }

    @Test
    void emitModificationReferencesChangedWhenGroupUuidNullOrReferencesEmpty() {
        String userId = "test-user";
        List<ModificationReferenceData> references = Collections.emptyList();
        notificationService.emitModificationReferencesChanged(ElementReferenceAction.CREATE, null, references, userId);
        verify(publisher, never()).send(anyString(), any());
    }
}
