/*
  Copyright (c) 2024, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author Antoine Bouhours <antoine.bouhours at rte-france.com>
 */
public class ModificationTypeTest {
    @Test
    public void testMaxStrategyNoneVsNone() {
        ModificationType noneStrategy1 = ModificationType.LOAD_CREATION;
        ModificationType noneStrategy2 = ModificationType.LOAD_MODIFICATION;
        assertEquals(noneStrategy1, noneStrategy1.maxStrategy(noneStrategy2));
        assertEquals(noneStrategy2, noneStrategy2.maxStrategy(noneStrategy1));
    }

    @Test
    public void testMaxStrategyNoneVsCollection() {
        ModificationType noneStrategy = ModificationType.LOAD_CREATION;
        ModificationType collectionStrategy = ModificationType.TABULAR_MODIFICATION;
        assertEquals(collectionStrategy, noneStrategy.maxStrategy(collectionStrategy));
        assertEquals(collectionStrategy, collectionStrategy.maxStrategy(noneStrategy));
    }

    @Test
    public void testMaxStrategyNoneVsAllCollectionsNeededForBusView() {
        ModificationType noneStrategy = ModificationType.LOAD_CREATION;
        ModificationType allCollectionsNeededForBusViewStrategy = ModificationType.VOLTAGE_INIT_MODIFICATION;
        assertEquals(allCollectionsNeededForBusViewStrategy, noneStrategy.maxStrategy(allCollectionsNeededForBusViewStrategy));
        assertEquals(allCollectionsNeededForBusViewStrategy, allCollectionsNeededForBusViewStrategy.maxStrategy(noneStrategy));
    }

    @Test
    public void testMaxStrategyCollectionVsCollection() {
        ModificationType collectionStrategy1 = ModificationType.TABULAR_MODIFICATION;
        ModificationType collectionStrategy2 = ModificationType.TABULAR_CREATION;
        assertEquals(collectionStrategy1, collectionStrategy1.maxStrategy(collectionStrategy2));
        assertEquals(collectionStrategy2, collectionStrategy2.maxStrategy(collectionStrategy1));
    }

    @Test
    public void testMaxStrategyCollectionVsAllCollectionsNeededForBusView() {
        ModificationType collectionStrategy = ModificationType.TABULAR_MODIFICATION;
        ModificationType allCollectionsNeededForBusViewStrategy = ModificationType.VOLTAGE_INIT_MODIFICATION;
        assertEquals(allCollectionsNeededForBusViewStrategy, collectionStrategy.maxStrategy(allCollectionsNeededForBusViewStrategy));
        assertEquals(allCollectionsNeededForBusViewStrategy, allCollectionsNeededForBusViewStrategy.maxStrategy(collectionStrategy));
    }
}
