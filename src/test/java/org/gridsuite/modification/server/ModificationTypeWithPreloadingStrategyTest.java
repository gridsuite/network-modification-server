/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.gridsuite.modification.server.modifications.ModificationTypeWithPreloadingStrategy;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
class ModificationTypeWithPreloadingStrategyTest {
    @Test
    void testMaxStrategyNoneVsNone() {
        ModificationTypeWithPreloadingStrategy noneStrategy1 = ModificationTypeWithPreloadingStrategy.LOAD_CREATION;
        ModificationTypeWithPreloadingStrategy noneStrategy2 = ModificationTypeWithPreloadingStrategy.LOAD_MODIFICATION;
        assertEquals(noneStrategy1, noneStrategy1.maxStrategy(noneStrategy2));
        assertEquals(noneStrategy2, noneStrategy2.maxStrategy(noneStrategy1));
    }

    @Test
    void testMaxStrategyNoneVsCollection() {
        ModificationTypeWithPreloadingStrategy noneStrategy = ModificationTypeWithPreloadingStrategy.LOAD_CREATION;
        ModificationTypeWithPreloadingStrategy collectionStrategy = ModificationTypeWithPreloadingStrategy.TABULAR_MODIFICATION;
        assertEquals(collectionStrategy, noneStrategy.maxStrategy(collectionStrategy));
        assertEquals(collectionStrategy, collectionStrategy.maxStrategy(noneStrategy));
    }

    @Test
    void testMaxStrategyNoneVsAllCollectionsNeededForBusView() {
        ModificationTypeWithPreloadingStrategy noneStrategy = ModificationTypeWithPreloadingStrategy.LOAD_CREATION;
        ModificationTypeWithPreloadingStrategy allCollectionsNeededForBusViewStrategy = ModificationTypeWithPreloadingStrategy.VOLTAGE_INIT_MODIFICATION;
        assertEquals(allCollectionsNeededForBusViewStrategy, noneStrategy.maxStrategy(allCollectionsNeededForBusViewStrategy));
        assertEquals(allCollectionsNeededForBusViewStrategy, allCollectionsNeededForBusViewStrategy.maxStrategy(noneStrategy));
    }

    @Test
    void testMaxStrategyCollectionVsCollection() {
        ModificationTypeWithPreloadingStrategy collectionStrategy1 = ModificationTypeWithPreloadingStrategy.TABULAR_MODIFICATION;
        ModificationTypeWithPreloadingStrategy collectionStrategy2 = ModificationTypeWithPreloadingStrategy.TABULAR_CREATION;
        assertEquals(collectionStrategy1, collectionStrategy1.maxStrategy(collectionStrategy2));
        assertEquals(collectionStrategy2, collectionStrategy2.maxStrategy(collectionStrategy1));
    }

    @Test
    void testMaxStrategyCollectionVsAllCollectionsNeededForBusView() {
        ModificationTypeWithPreloadingStrategy collectionStrategy = ModificationTypeWithPreloadingStrategy.TABULAR_MODIFICATION;
        ModificationTypeWithPreloadingStrategy allCollectionsNeededForBusViewStrategy = ModificationTypeWithPreloadingStrategy.VOLTAGE_INIT_MODIFICATION;
        assertEquals(allCollectionsNeededForBusViewStrategy, collectionStrategy.maxStrategy(allCollectionsNeededForBusViewStrategy));
        assertEquals(allCollectionsNeededForBusViewStrategy, allCollectionsNeededForBusViewStrategy.maxStrategy(collectionStrategy));
    }
}
