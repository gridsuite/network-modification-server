/*
  Copyright (c) 2026, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.powsybl.network.store.client.PreloadingStrategy;
import lombok.Getter;
import org.gridsuite.modification.ModificationType;

import java.util.HashMap;
import java.util.Map;

/**
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
@Getter
public enum ModificationTypeWithPreloadingStrategy {
    EQUIPMENT_ATTRIBUTE_MODIFICATION(ModificationType.EQUIPMENT_ATTRIBUTE_MODIFICATION, PreloadingStrategy.NONE),
    LOAD_CREATION(ModificationType.LOAD_CREATION, PreloadingStrategy.NONE),
    LOAD_MODIFICATION(ModificationType.LOAD_MODIFICATION, PreloadingStrategy.NONE),
    BALANCES_ADJUSTMENT_MODIFICATION(ModificationType.BALANCES_ADJUSTMENT_MODIFICATION, PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW),
    BATTERY_CREATION(ModificationType.BATTERY_CREATION, PreloadingStrategy.NONE),
    BATTERY_MODIFICATION(ModificationType.BATTERY_MODIFICATION, PreloadingStrategy.NONE),
    GENERATOR_CREATION(ModificationType.GENERATOR_CREATION, PreloadingStrategy.NONE),
    GENERATOR_MODIFICATION(ModificationType.GENERATOR_MODIFICATION, PreloadingStrategy.NONE),
    EQUIPMENT_DELETION(ModificationType.EQUIPMENT_DELETION, PreloadingStrategy.NONE),
    BY_FILTER_DELETION(ModificationType.BY_FILTER_DELETION, PreloadingStrategy.COLLECTION),
    LINE_CREATION(ModificationType.LINE_CREATION, PreloadingStrategy.NONE),
    LINE_MODIFICATION(ModificationType.LINE_MODIFICATION, PreloadingStrategy.NONE),
    TWO_WINDINGS_TRANSFORMER_CREATION(ModificationType.TWO_WINDINGS_TRANSFORMER_CREATION, PreloadingStrategy.NONE),
    TWO_WINDINGS_TRANSFORMER_MODIFICATION(ModificationType.TWO_WINDINGS_TRANSFORMER_MODIFICATION, PreloadingStrategy.NONE),
    GROOVY_SCRIPT(ModificationType.GROOVY_SCRIPT, PreloadingStrategy.COLLECTION),
    SUBSTATION_CREATION(ModificationType.SUBSTATION_CREATION, PreloadingStrategy.NONE),
    SUBSTATION_MODIFICATION(ModificationType.SUBSTATION_MODIFICATION, PreloadingStrategy.NONE),
    SHUNT_COMPENSATOR_CREATION(ModificationType.SHUNT_COMPENSATOR_CREATION, PreloadingStrategy.NONE),
    SHUNT_COMPENSATOR_MODIFICATION(ModificationType.SHUNT_COMPENSATOR_MODIFICATION, PreloadingStrategy.NONE),
    STATIC_VAR_COMPENSATOR_CREATION(ModificationType.STATIC_VAR_COMPENSATOR_CREATION, PreloadingStrategy.NONE),
    VOLTAGE_LEVEL_CREATION(ModificationType.VOLTAGE_LEVEL_CREATION, PreloadingStrategy.NONE),
    VOLTAGE_LEVEL_MODIFICATION(ModificationType.VOLTAGE_LEVEL_MODIFICATION, PreloadingStrategy.NONE),
    LINE_SPLIT_WITH_VOLTAGE_LEVEL(ModificationType.LINE_SPLIT_WITH_VOLTAGE_LEVEL, PreloadingStrategy.NONE),
    LINE_ATTACH_TO_VOLTAGE_LEVEL(ModificationType.LINE_ATTACH_TO_VOLTAGE_LEVEL, PreloadingStrategy.NONE),
    LINES_ATTACH_TO_SPLIT_LINES(ModificationType.LINES_ATTACH_TO_SPLIT_LINES, PreloadingStrategy.NONE),
    GENERATOR_SCALING(ModificationType.GENERATOR_SCALING, PreloadingStrategy.COLLECTION),
    LOAD_SCALING(ModificationType.LOAD_SCALING, PreloadingStrategy.COLLECTION),
    OPERATING_STATUS_MODIFICATION(ModificationType.OPERATING_STATUS_MODIFICATION, PreloadingStrategy.NONE),
    DELETE_VOLTAGE_LEVEL_ON_LINE(ModificationType.DELETE_VOLTAGE_LEVEL_ON_LINE, PreloadingStrategy.NONE),
    DELETE_ATTACHING_LINE(ModificationType.DELETE_ATTACHING_LINE, PreloadingStrategy.NONE),
    GENERATION_DISPATCH(ModificationType.GENERATION_DISPATCH, PreloadingStrategy.COLLECTION),
    VOLTAGE_INIT_MODIFICATION(ModificationType.VOLTAGE_INIT_MODIFICATION, PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW),
    VSC_CREATION(ModificationType.VSC_CREATION, PreloadingStrategy.NONE),
    VSC_MODIFICATION(ModificationType.VSC_MODIFICATION, PreloadingStrategy.NONE),
    CONVERTER_STATION_CREATION(ModificationType.CONVERTER_STATION_CREATION, PreloadingStrategy.NONE),
    CONVERTER_STATION_MODIFICATION(ModificationType.CONVERTER_STATION_MODIFICATION, PreloadingStrategy.NONE),
    TABULAR_MODIFICATION(ModificationType.TABULAR_MODIFICATION, PreloadingStrategy.COLLECTION),
    TABULAR_CREATION(ModificationType.TABULAR_CREATION, PreloadingStrategy.COLLECTION),
    BY_FORMULA_MODIFICATION(ModificationType.BY_FORMULA_MODIFICATION, PreloadingStrategy.COLLECTION),
    MODIFICATION_BY_ASSIGNMENT(ModificationType.MODIFICATION_BY_ASSIGNMENT, PreloadingStrategy.COLLECTION),
    COMPOSITE_MODIFICATION(ModificationType.COMPOSITE_MODIFICATION, PreloadingStrategy.COLLECTION),
    LCC_CONVERTER_STATION_CREATION(ModificationType.LCC_CONVERTER_STATION_CREATION, PreloadingStrategy.NONE),
    LCC_CONVERTER_STATION_MODIFICATION(ModificationType.LCC_CONVERTER_STATION_MODIFICATION, PreloadingStrategy.NONE),
    LCC_CREATION(ModificationType.LCC_CREATION, PreloadingStrategy.NONE),
    LCC_MODIFICATION(ModificationType.LCC_MODIFICATION, PreloadingStrategy.NONE),
    VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION(ModificationType.VOLTAGE_LEVEL_TOPOLOGY_MODIFICATION, PreloadingStrategy.NONE),
    CREATE_COUPLING_DEVICE(ModificationType.CREATE_COUPLING_DEVICE, PreloadingStrategy.NONE),
    CREATE_VOLTAGE_LEVEL_TOPOLOGY(ModificationType.CREATE_VOLTAGE_LEVEL_TOPOLOGY, PreloadingStrategy.NONE),
    LIMIT_SETS_TABULAR_MODIFICATION(ModificationType.LIMIT_SETS_TABULAR_MODIFICATION, PreloadingStrategy.COLLECTION),
    CREATE_VOLTAGE_LEVEL_SECTION(ModificationType.CREATE_VOLTAGE_LEVEL_SECTION, PreloadingStrategy.NONE),
    MOVE_VOLTAGE_LEVEL_FEEDER_BAYS(ModificationType.MOVE_VOLTAGE_LEVEL_FEEDER_BAYS, PreloadingStrategy.NONE);

    private final ModificationType type;
    private final PreloadingStrategy strategy;
    private static final Map<ModificationType, ModificationTypeWithPreloadingStrategy> TYPE_MAP;

    static {
        TYPE_MAP = new HashMap<>();
        for (ModificationTypeWithPreloadingStrategy value : values()) {
            TYPE_MAP.put(value.type, value);
        }
    }

    ModificationTypeWithPreloadingStrategy(ModificationType type, PreloadingStrategy strategy) {
        this.type = type;
        this.strategy = strategy;
    }

    public ModificationTypeWithPreloadingStrategy maxStrategy(ModificationTypeWithPreloadingStrategy other) {
        return switch (strategy) {
            case NONE -> {
                if (other.strategy != PreloadingStrategy.NONE) {
                    yield other;
                }
                yield this;
            }
            case COLLECTION -> {
                if (other.strategy == PreloadingStrategy.ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW) {
                    yield other;
                }
                yield this;
            }
            case ALL_COLLECTIONS_NEEDED_FOR_BUS_VIEW -> this;
        };
    }

    public static ModificationTypeWithPreloadingStrategy fromModificationType(ModificationType modificationType) {
        ModificationTypeWithPreloadingStrategy result = TYPE_MAP.get(modificationType);
        if (result == null) {
            throw new IllegalArgumentException("No ModificationTypeWithPreloadingStrategy found for: " + modificationType);
        }
        return result;
    }
}
