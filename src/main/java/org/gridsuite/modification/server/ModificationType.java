/**
 * Copyright (c) 2021, All partners of the iTesla project (http://www.itesla-project.eu/consortium)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

/**
 * @author Slimane Amar <slimane.amar at rte-france.com>
 * @author Franck Lecuyer <franck.lecuyer at rte-france.com>
 */
public enum ModificationType {
    EQUIPMENT_ATTRIBUTE_MODIFICATION,
    LOAD_CREATION,
    LOAD_MODIFICATION,
    GENERATOR_CREATION,
    GENERATOR_MODIFICATION,
    EQUIPMENT_DELETION,
    LINE_CREATION,
    TWO_WINDINGS_TRANSFORMER_CREATION,
    GROOVY_SCRIPT,
    SUBSTATION_CREATION,
    SHUNT_COMPENSATOR_CREATION,
    VOLTAGE_LEVEL_CREATION,
    LINE_SPLIT_WITH_VOLTAGE_LEVEL,
    LINE_ATTACH_TO_VOLTAGE_LEVEL,
    LINES_ATTACH_TO_SPLIT_LINES,
    BRANCH_STATUS_MODIFICATION,
    DELETE_VOLTAGE_LEVEL_ON_LINE,
    DELETE_ATTACHING_LINE
}
