/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.FilterWithDistributionKeysLoader;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import java.util.Map;
import java.util.UUID;

/**
 * Adapts the filter server client to the loader contract expected by the modification library.
 *
 * @author Kamil MARUT <kamil.marut at rte-france.com>
 */
@Service
public class FilterWithDistributionKeysLoaderService {
    private static final Logger LOGGER = LoggerFactory.getLogger(FilterWithDistributionKeysLoaderService.class);

    private final FilterService filterService;

    private final DirectoryService directoryService;

    public FilterWithDistributionKeysLoaderService(FilterService filterService, DirectoryService directoryService) {
        this.filterService = filterService;
        this.directoryService = directoryService;
    }

    public FilterWithDistributionKeysLoader getLoader() {
        return filterService::getStandaloneFiltersWithDistributionKeys;
    }

    public FilterWithDistributionKeysLoader getLoaderWithNames() {
        return filterUuids -> {
            Map<UUID, FilterWithDistributionKeys> filters = filterService.getStandaloneFiltersWithDistributionKeys(filterUuids);
            try {
                Map<UUID, String> names = directoryService.getElementNames(filters.keySet());
                filters.forEach((filterUuid, filter) -> {
                    if (filter.getFilter() != null) {
                        filter.getFilter().setName(names.get(filterUuid));
                    }
                });
            } catch (RestClientException e) {
                LOGGER.warn("Could not resolve filter names from the directory, they will be numbered instead", e);
            }
            return filters;
        };
    }
}
