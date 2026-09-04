/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.ModificationContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class ModificationContextFactoryTest {

    private FilterLoaderService filterLoaderService;
    private LoadFlowParametersLoaderService loadFlowParametersLoaderService;
    private ModificationContextFactory factory;

    @BeforeEach
    void setUp() {
        filterLoaderService = mock(FilterLoaderService.class);
        loadFlowParametersLoaderService = mock(LoadFlowParametersLoaderService.class);
        factory = new ModificationContextFactory(filterLoaderService, loadFlowParametersLoaderService);
    }

    @Test
    void createWiresEveryLoader() {
        ModificationContext context = factory.create();

        assertSame(filterLoaderService, context.filterLoader());
        assertSame(loadFlowParametersLoaderService, context.loadFlowParametersLoader());
    }

    @Test
    void createReturnsAFreshContextEachTime() {
        assertNotSame(factory.create(), factory.create(),
                "A context is short-lived and must never be shared as a singleton");
    }
}
