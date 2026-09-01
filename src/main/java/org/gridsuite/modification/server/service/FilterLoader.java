/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.filter.wip.Filter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.UUID;

/**
 * @author joris mancini <joris.mancini_externe at rte-france.com>
 */
@Service
public class FilterLoader implements org.gridsuite.filter.wip.FilterLoader {

    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private static String filterServerBaseUri;

    private final RestTemplate restTemplate;

    public FilterLoader(@Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}") String filterServerBaseUri,
                        RestTemplate restTemplate) {
        setFilterServerBaseUri(filterServerBaseUri);
        this.restTemplate = restTemplate;
    }

    public static void setFilterServerBaseUri(String filterServerBaseUri) {
        FilterLoader.filterServerBaseUri = filterServerBaseUri;
    }

    @Override
    public List<Filter> load(List<UUID> filtersUuids) {
        if (filtersUuids == null || filtersUuids.isEmpty()) {
            return List.of();
        }
        String path = UriComponentsBuilder.fromPath(DELIMITER
                        + FILTER_SERVER_API_VERSION
                        + "/standalone-filters")
                .queryParam("ids", filtersUuids)
                .buildAndExpand()
                .toUriString();
        return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<Filter>>() { }).getBody();
    }
}
