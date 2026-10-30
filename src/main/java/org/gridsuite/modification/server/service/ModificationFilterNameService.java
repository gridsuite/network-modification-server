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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
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

    public void addFilterNames(ModificationInfos modification) {
        addFilterNames(List.of(modification));
    }

    public void addFilterNames(List<ModificationInfos> modifications) {
        List<FilterInfos> filters = new ArrayList<>();
        modifications.forEach(modification -> getFiltersStream(modification, filters));
        if (filters.isEmpty()) {
            return;
        }
        Set<UUID> filterUuids = filters.stream().map(FilterInfos::getId).collect(Collectors.toSet());
        Map<UUID, String> names = directoryService.getElementNames(filterUuids);
        filters.forEach(filter -> filter.setName(names.get(filter.getId())));
    }

    private static void getFiltersStream(ModificationInfos modification, List<FilterInfos> filters) {
        modification.getFiltersStream().forEach(filters::add);
        contentOf(modification).forEach(content -> getFiltersStream(content, filters));
    }
}
