/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.elasticsearch.NoSuchIndexException;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;


/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
class NetworkVariantsListenerTests {
    private static final UUID NETWORK_UUID = UUID.randomUUID();
    private static final String VARIANT_ID = "variant_1";

    @Autowired
    private EquipmentInfosService equipmentInfosService;

    @AfterEach
    void tearDown() {
        try {
            equipmentInfosService.deleteAll();
        } catch (NoSuchIndexException ex) {
            // no need to worry that much
        }
    }

    @Test
    void testVariantNotifications() {
        NetworkVariantsListener listener = new NetworkVariantsListener(null, NETWORK_UUID, equipmentInfosService);

        listener.onVariantRemoved(VARIANT_ID);
        listener.onVariantCreated("variant_1", "variant_2");
        assertEquals(0, equipmentInfosService.findEquipmentInfosList(List.of("equipment1", "equipment2"), NETWORK_UUID, "variant_2").size());
        listener.onVariantOverwritten("variant_2", "variant_3");
        listener.onUpdate(null, null, null, null, null);
        listener.onExtensionUpdate(null, null, null, null, null);
        listener.onPropertyAdded(null, null, null);
        listener.onPropertyReplaced(null, null, null, null);
        listener.onPropertyRemoved(null, null, null);
    }
}
