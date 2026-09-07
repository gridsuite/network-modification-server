/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.LoadFlowParametersInfos;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class LoadFlowParametersLoaderServiceTest {

    private LoadFlowService loadFlowService;
    private LoadFlowParametersLoaderService loadFlowParametersLoaderService;

    @BeforeEach
    void setUp() {
        loadFlowService = mock(LoadFlowService.class);
        loadFlowParametersLoaderService = new LoadFlowParametersLoaderService(loadFlowService);
    }

    @Test
    void loadWrapsTheParametersReturnedByTheLoadFlowServerClient() {
        UUID parametersUuid = UUID.randomUUID();
        LoadFlowParametersInfos parameters = LoadFlowParametersInfos.builder().provider("OpenLoadFlow").build();
        when(loadFlowService.getLoadFlowParametersInfos(parametersUuid)).thenReturn(parameters);

        Optional<LoadFlowParametersInfos> loaded = loadFlowParametersLoaderService.load(parametersUuid);

        assertTrue(loaded.isPresent());
        assertEquals("OpenLoadFlow", loaded.orElseThrow().getProvider());
    }

    @Test
    void loadReturnsEmptyWhenTheParametersDoNotExistAnymore() {
        UUID parametersUuid = UUID.randomUUID();
        when(loadFlowService.getLoadFlowParametersInfos(parametersUuid)).thenReturn(null);

        assertTrue(loadFlowParametersLoaderService.load(parametersUuid).isEmpty(),
                "Absent parameters must surface as an empty Optional, not as a null");
    }
}
