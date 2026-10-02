/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.dto;

/**
 * Selects the modifications to keep according to their stashed state
 *
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
public enum StashedFilter {
    ALL,
    STASHED,
    UNSTASHED;

    public boolean accepts(Boolean stashed) {
        return switch (this) {
            case ALL -> true;
            case STASHED -> Boolean.TRUE.equals(stashed);
            case UNSTASHED -> !Boolean.TRUE.equals(stashed);
        };
    }
}
