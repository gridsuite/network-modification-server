/*
  Copyright (c) 2021, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.powsybl.network.store.client.PreloadingStrategy;

import static org.gridsuite.modification.server.NetworkModificationException.Type.PRELOADING_STRATEGY_NOT_ALLOWED;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public enum ModificationType {
    EQUIPMENT_ATTRIBUTE_MODIFICATION(PreloadingStrategy.NONE),
    LOAD_CREATION(PreloadingStrategy.NONE),
    LOAD_MODIFICATION(PreloadingStrategy.NONE),
    BATTERY_CREATION(PreloadingStrategy.NONE),
    BATTERY_MODIFICATION(PreloadingStrategy.NONE),
    GENERATOR_CREATION(PreloadingStrategy.NONE),
    GENERATOR_MODIFICATION(PreloadingStrategy.NONE),
    EQUIPMENT_DELETION(PreloadingStrategy.NONE),
    LINE_CREATION(PreloadingStrategy.NONE),
    LINE_MODIFICATION(PreloadingStrategy.NONE),
    TWO_WINDINGS_TRANSFORMER_CREATION(PreloadingStrategy.NONE),
    TWO_WINDINGS_TRANSFORMER_MODIFICATION(PreloadingStrategy.NONE),
    GROOVY_SCRIPT(PreloadingStrategy.COLLECTION),
    SUBSTATION_CREATION(PreloadingStrategy.NONE),
    SUBSTATION_MODIFICATION(PreloadingStrategy.NONE),
    SHUNT_COMPENSATOR_CREATION(PreloadingStrategy.NONE),
    SHUNT_COMPENSATOR_MODIFICATION(PreloadingStrategy.NONE),
    VOLTAGE_LEVEL_CREATION(PreloadingStrategy.NONE),
    VOLTAGE_LEVEL_MODIFICATION(PreloadingStrategy.NONE),
    LINE_SPLIT_WITH_VOLTAGE_LEVEL(PreloadingStrategy.NONE),
    LINE_ATTACH_TO_VOLTAGE_LEVEL(PreloadingStrategy.NONE),
    LINES_ATTACH_TO_SPLIT_LINES(PreloadingStrategy.NONE),
    GENERATOR_SCALING(PreloadingStrategy.COLLECTION),
    LOAD_SCALING(PreloadingStrategy.COLLECTION),
    BRANCH_STATUS_MODIFICATION(PreloadingStrategy.NONE),
    DELETE_VOLTAGE_LEVEL_ON_LINE(PreloadingStrategy.NONE),
    DELETE_ATTACHING_LINE(PreloadingStrategy.NONE),
    GENERATION_DISPATCH(PreloadingStrategy.COLLECTION);

    private final PreloadingStrategy strategy;

    ModificationType(PreloadingStrategy strategy) {
        this.strategy = strategy;
    }

    public PreloadingStrategy getStrategy() {
        return strategy;
    }

    public ModificationType maxStrategy(ModificationType other) {
        if (strategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW || other.strategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
            throw new NetworkModificationException(PRELOADING_STRATEGY_NOT_ALLOWED, "Preloading strategy ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW not allowed");
        }
        return strategy != PreloadingStrategy.NONE ? this : other;
    }
}
