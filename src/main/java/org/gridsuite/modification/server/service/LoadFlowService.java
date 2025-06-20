/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import lombok.extern.slf4j.Slf4j;
import org.gridsuite.modification.ILoadFlowService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.LoadFlowParametersInfos;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.UUID;

import static org.gridsuite.modification.server.NetworkModificationServerException.handleChangeError;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Slf4j
@Service
public class LoadFlowService implements ILoadFlowService {
    private static final String LOADFLOW_SERVER_API_VERSION = "v1";
    private static final String DELIMITER = "/";
    private static final String PARAMETERS_URI = "/parameters/{parametersUuid}";

    private final String loadFlowServerBaseUri;
    private final RestTemplate restTemplate;

    public LoadFlowService(@Value("${gridsuite.services.loadflow-server.base-uri:http://loadflow-server/}") String loadFlowServerBaseUri, RestTemplate restTemplate) {
        this.loadFlowServerBaseUri = loadFlowServerBaseUri;
        this.restTemplate = restTemplate;
    }

    @Override
    public LoadFlowParametersInfos getLoadFlowParametersInfos(UUID uuid) {
        String path = UriComponentsBuilder.fromPath(DELIMITER + LOADFLOW_SERVER_API_VERSION + PARAMETERS_URI)
                .buildAndExpand(uuid).toUriString();
        try {
            return restTemplate.getForObject(loadFlowServerBaseUri + path, LoadFlowParametersInfos.class);
        } catch (HttpStatusCodeException e) {
            if (e.getStatusCode() == HttpStatus.NOT_FOUND) {
                log.error("Load flow parameters with UUID {} not found", uuid);
                throw new NetworkModificationException(NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_NOT_FOUND, "Load flow parameters not found for UUID: " + uuid);
            } else {
                throw handleChangeError(e, NetworkModificationException.Type.LOAD_FLOW_PARAMETERS_FETCH_ERROR);
            }
        }
    }
}
