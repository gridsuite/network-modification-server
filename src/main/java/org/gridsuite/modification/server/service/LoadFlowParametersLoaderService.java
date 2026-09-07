/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.context.LoadFlowParametersLoader;
import org.gridsuite.modification.dto.LoadFlowParametersInfos;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.UUID;

/**
 * Adapts the load flow server client to the loader contract expected by the modification library.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Service
public class LoadFlowParametersLoaderService implements LoadFlowParametersLoader {

    private final LoadFlowService loadFlowService;

    public LoadFlowParametersLoaderService(LoadFlowService loadFlowService) {
        this.loadFlowService = loadFlowService;
    }

    @Override
    public Optional<LoadFlowParametersInfos> load(UUID parametersUuid) {
        return Optional.ofNullable(loadFlowService.getLoadFlowParametersInfos(parametersUuid));
    }
}
