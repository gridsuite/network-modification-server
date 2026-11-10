/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.FilterLoader;
import org.gridsuite.modification.context.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.context.ModificationContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class ModificationContextFactoryTest {

    private FilterLoaderService filterLoaderService;
    private FilterWithDistributionKeysLoaderService filterWithDistributionKeysLoaderService;
    private LoadFlowParametersLoaderService loadFlowParametersLoaderService;
    private ModificationContextFactory factory;

    @BeforeEach
    void setUp() {
        filterLoaderService = mock(FilterLoaderService.class);
        filterWithDistributionKeysLoaderService = mock(FilterWithDistributionKeysLoaderService.class);
        loadFlowParametersLoaderService = mock(LoadFlowParametersLoaderService.class);
        factory = new ModificationContextFactory(filterLoaderService, filterWithDistributionKeysLoaderService, loadFlowParametersLoaderService);
    }

    @Test
    void createWiresEveryLoader() {
        FilterLoader filterLoader = filterUuids -> Map.of();
        FilterWithDistributionKeysLoader filterWithDistributionKeysLoader = filterUuids -> Map.of();
        when(filterLoaderService.getLoader()).thenReturn(filterLoader);
        when(filterWithDistributionKeysLoaderService.getLoader()).thenReturn(filterWithDistributionKeysLoader);

        ModificationContext context = factory.create();

        assertSame(filterLoader, context.filterLoader());
        assertSame(filterWithDistributionKeysLoader, context.filterWithDistributionKeysLoader());
        assertSame(loadFlowParametersLoaderService, context.loadFlowParametersLoader());
    }

    @Test
    void createWithFilterNamesWiresTheLoadersWithNames() {
        FilterLoader filterLoader = filterUuids -> Map.of();
        FilterWithDistributionKeysLoader filterWithDistributionKeysLoader = filterUuids -> Map.of();
        when(filterLoaderService.getLoaderWithNames()).thenReturn(filterLoader);
        when(filterWithDistributionKeysLoaderService.getLoaderWithNames()).thenReturn(filterWithDistributionKeysLoader);

        ModificationContext context = factory.createWithFilterNames();

        assertSame(filterLoader, context.filterLoader());
        assertSame(filterWithDistributionKeysLoader, context.filterWithDistributionKeysLoader());
        assertSame(loadFlowParametersLoaderService, context.loadFlowParametersLoader());
    }

    @Test
    void createReturnsAFreshContextEachTime() {
        assertNotSame(factory.create(), factory.create(),
                "A context is short-lived and must never be shared as a singleton");
    }
}
