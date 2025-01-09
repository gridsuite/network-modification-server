/*
 * Copyright (c) 2024, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.BuildException;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.messaging.support.MessageBuilder;

import static org.junit.jupiter.api.Assertions.assertThrows;

@SpringBootTest
class BuildWorkerServiceTest {

    @Autowired
    private BuildWorkerService buildWorkerService;

    @Test
    void testConsumeBuildWithMalformedInput() {
        assertThrows(
            BuildException.class,
            () -> buildWorkerService.consumeBuild().accept(MessageBuilder.withPayload("wrong message").build()),
            "Failed to read build message");
    }
}
