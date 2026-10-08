/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Adapts the filter server client to the loader contract expected by the modification library.
 *
 * @author Kamil MARUT <kamil.marut at rte-france.com>
 */
@Service
public class FilterWithDistributionKeysLoaderService implements FilterWithDistributionKeysLoader {

    private final FilterService filterService;

    public FilterWithDistributionKeysLoaderService(FilterService filterService) {
        this.filterService = filterService;
    }

    @Override
    public Map<UUID, FilterWithDistributionKeys> load(List<UUID> filterUuids) {
        return filterService.getStandaloneFiltersWithDistributionKeys(filterUuids);
    }
}
