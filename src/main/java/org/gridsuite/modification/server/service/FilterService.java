/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClient;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Service
public class FilterService implements IFilterService {

    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private static final String STANDALONE_FILTERS_URI = "/standalone-filters";

    private static final String WITH_DISTRIBUTION_KEYS_URI = STANDALONE_FILTERS_URI + "/with-distribution-keys";

    private static final String IDS_PARAM = "ids";

    private static final ParameterizedTypeReference<Map<UUID, Filter>> STANDALONE_FILTERS_BY_ID = new ParameterizedTypeReference<>() { };

    private static final ParameterizedTypeReference<Map<UUID, FilterWithDistributionKeys>> STANDALONE_FILTERS_WITH_DISTRIBUTION_KEYS_BY_ID = new ParameterizedTypeReference<>() { };

    private static String filterServerBaseUri;

    private final RestClient restClient;

    public FilterService(@Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}") String filterServerBaseUri,
                         RestClient restClient) {
        setFilterServerBaseUri(filterServerBaseUri);
        this.restClient = restClient;
    }

    public static void setFilterServerBaseUri(String filterServerBaseUri) {
        FilterService.filterServerBaseUri = filterServerBaseUri;
    }

    public List<AbstractFilter> getFilters(List<UUID> filtersUuids) {
        var ids = !filtersUuids.isEmpty() ? "?ids=" + filtersUuids.stream().map(UUID::toString).collect(Collectors.joining(",")) : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/metadata" + ids)
            .buildAndExpand()
            .toUriString();
        return restClient.get()
            .uri(filterServerBaseUri + path)
            .retrieve()
            .body(new ParameterizedTypeReference<>() { });
    }

    /**
     * Retrieves self-contained filter definitions, which can then be evaluated locally against a network.
     *
     * @return the filters found, indexed by their identifier; identifiers with no matching filter are omitted
     */
    public Map<UUID, Filter> getStandaloneFilters(List<UUID> filtersUuids) {
        if (CollectionUtils.isEmpty(filtersUuids)) {
            return Map.of();
        }
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + STANDALONE_FILTERS_URI)
                .queryParam(IDS_PARAM, filtersUuids)
                .buildAndExpand()
                .toUriString();
        Map<UUID, Filter> filters = restClient.get()
                .uri(filterServerBaseUri + path)
                .retrieve()
                .body(STANDALONE_FILTERS_BY_ID);
        return filters == null ? Map.of() : filters;
    }

    /**
     * Retrieves self-contained filter definitions along with the distribution key of each equipment they
     * select, which can then be evaluated locally against a network.
     *
     * @return the filters found, indexed by their identifier; identifiers with no matching filter are omitted
     */
    public Map<UUID, FilterWithDistributionKeys> getStandaloneFiltersWithDistributionKeys(List<UUID> filtersUuids) {
        if (CollectionUtils.isEmpty(filtersUuids)) {
            return Map.of();
        }
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + WITH_DISTRIBUTION_KEYS_URI)
                .queryParam(IDS_PARAM, filtersUuids)
                .buildAndExpand()
                .toUriString();
        Map<UUID, FilterWithDistributionKeys> filters = restClient.get()
                .uri(filterServerBaseUri + path)
                .retrieve()
                .body(STANDALONE_FILTERS_WITH_DISTRIBUTION_KEYS_BY_ID);
        return filters == null ? Map.of() : filters;
    }
}
