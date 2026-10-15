/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.utils;

import org.gridsuite.filter.wip.Filter;

import java.util.Map;
import java.util.UUID;

/**
 * @author Kamil MARUT <kamil.marut at rte-france.com>
 */
public record FilterWithDistributionKeysStub(UUID id, Filter filter, Map<String, Double> distributionKeys) {
}
