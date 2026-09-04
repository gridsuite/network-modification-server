/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.ModificationContext;
import org.springframework.stereotype.Service;

/**
 * Builds the context from which modifications resolve their dependencies.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Service
public class ModificationContextFactory {

    private final FilterLoaderService filterLoaderService;
    private final LoadFlowParametersLoaderService loadFlowParametersLoaderService;

    public ModificationContextFactory(FilterLoaderService filterLoaderService,
                                      LoadFlowParametersLoaderService loadFlowParametersLoaderService) {
        this.filterLoaderService = filterLoaderService;
        this.loadFlowParametersLoaderService = loadFlowParametersLoaderService;
    }

    public ModificationContext create() {
        return ModificationContext.builder()
                .filterLoader(filterLoaderService)
                .loadFlowParametersLoader(loadFlowParametersLoaderService)
                .build();
    }
}
