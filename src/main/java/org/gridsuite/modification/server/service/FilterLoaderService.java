/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.FilterLoader;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * Loads self-contained filter definitions from the filter server.
 *
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
@Service
public class FilterLoaderService implements FilterLoader {

    private static final String FILTER_SERVER_API_VERSION = "v1";
    private static final String DELIMITER = "/";
    private static final String STANDALONE_FILTERS_URI = "/standalone-filters";
    private static final String IDS_PARAM = "ids";
    private static final ParameterizedTypeReference<Map<UUID, Filter>> FILTERS_BY_ID = new ParameterizedTypeReference<>() { };

    private final String filterServerBaseUri;
    private final RestTemplate restTemplate;

    public FilterLoaderService(@Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}") String filterServerBaseUri,
                               RestTemplate restTemplate) {
        this.filterServerBaseUri = filterServerBaseUri;
        this.restTemplate = restTemplate;
    }

    @Override
    public Map<UUID, Filter> load(List<UUID> filterUuids) {
        if (filterUuids == null || filterUuids.isEmpty()) {
            return Map.of();
        }
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + STANDALONE_FILTERS_URI)
                .queryParam(IDS_PARAM, filterUuids)
                .buildAndExpand()
                .toUriString();
        Map<UUID, Filter> filters = restTemplate
                .exchange(filterServerBaseUri + path, HttpMethod.GET, null, FILTERS_BY_ID)
                .getBody();
        return filters == null ? Map.of() : filters;
    }
}
