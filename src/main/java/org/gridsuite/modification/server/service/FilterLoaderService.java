/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.context.FilterLoader;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.UUID;

/**
 * Adapts the filter server client to the loader contract expected by the modification library, naming the filters
 * after the directory, which owns their names.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Service
public class FilterLoaderService {

    private final FilterService filterService;

    private final DirectoryService directoryService;

    public FilterLoaderService(FilterService filterService, DirectoryService directoryService) {
        this.filterService = filterService;
        this.directoryService = directoryService;
    }

    public FilterLoader getLoader() {
        return filterService::getStandaloneFilters;
    }

    public FilterLoader getLoaderWithNames() {
        return filterUuids -> {
            Map<UUID, Filter> filters = filterService.getStandaloneFilters(filterUuids);
            Map<UUID, String> names = directoryService.getElementNames(filters.keySet());
            filters.forEach((filterUuid, filter) -> filter.setName(names.get(filterUuid)));
            return filters;
        };
    }
}
