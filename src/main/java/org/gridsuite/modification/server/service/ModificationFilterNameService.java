/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.gridsuite.modification.server.utils.ModificationInfosUtils.contentOf;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
@Service
public class ModificationFilterNameService {
    private static final Logger LOGGER = LoggerFactory.getLogger(ModificationFilterNameService.class);

    private final DirectoryService directoryService;

    public ModificationFilterNameService(DirectoryService directoryService) {
        this.directoryService = directoryService;
    }

    /**
     * Sets, on the filters the given modification references, their current name in the directory.
     */
    public void addFilterNames(ModificationInfos modification) {
        resolveFilterNames(List.of(modification));
    }

    /**
     * Sets, on the filters the given modifications reference, their current name in the directory.
     */
    public void addFilterNames(List<ModificationInfos> modifications) {
        resolveFilterNames(modifications);
    }

    /**
     * Sets, on the filters the given modifications reference, their current name in the directory, before applying them.
     * <p>
     * Filter names only feed the reports here: failing to resolve them must not prevent applying the modifications,
     * the reports then show the filter ids instead.
     */
    public void addFilterNamesForReports(List<ModificationInfos> modifications) {
        try {
            resolveFilterNames(modifications);
        } catch (RestClientException e) {
            LOGGER.warn("Could not resolve filter names from the directory, reports will show filter ids instead", e);
        }
    }

    /**
     * Reads the name of every filter the given modifications reference, nested ones included, in a single call to the
     * directory-server: the directory owns filter names, they are not stored. A filter the directory does not know
     * anymore (deleted) gets a null name.
     * <p>
     * Remote call: never make it inside a transaction.
     */
    private void resolveFilterNames(Collection<ModificationInfos> modifications) {
        List<FilterInfos> filters = new ArrayList<>();
        modifications.forEach(modification -> collectFilters(modification, filters));
        if (filters.isEmpty()) {
            return;
        }
        Map<UUID, String> names = directoryService.getElementNames(filters.stream().map(FilterInfos::getId).collect(Collectors.toSet()));
        filters.forEach(filter -> filter.setName(names.get(filter.getId())));
    }

    private static void collectFilters(ModificationInfos modification, List<FilterInfos> filters) {
        modification.collectFilters().forEach(filters::add);
        contentOf(modification).forEach(content -> collectFilters(content, filters));
    }
}
