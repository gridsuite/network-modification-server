/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.Network;
import org.apache.commons.collections4.CollectionUtils;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.utils.FilterServiceUtils;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Service
public class FilterService implements IFilterService {

    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private static final String STANDALONE_FILTERS_URI = "/standalone-filters";

    private static final String IDS_PARAM = "ids";

    private static final ParameterizedTypeReference<Map<UUID, Filter>> STANDALONE_FILTERS_BY_ID = new ParameterizedTypeReference<>() { };

    private static String filterServerBaseUri;

    private final RestTemplate restTemplate;

    public FilterService(@Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}") String filterServerBaseUri,
                         RestTemplate restTemplate) {
        setFilterServerBaseUri(filterServerBaseUri);
        this.restTemplate = restTemplate;
    }

    public static void setFilterServerBaseUri(String filterServerBaseUri) {
        FilterService.filterServerBaseUri = filterServerBaseUri;
    }

    public List<AbstractFilter> getFilters(List<UUID> filtersUuids) {
        var ids = !filtersUuids.isEmpty() ? "?ids=" + filtersUuids.stream().map(UUID::toString).collect(Collectors.joining(",")) : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/metadata" + ids)
            .buildAndExpand()
            .toUriString();
        return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<AbstractFilter>>() { }).getBody();
    }

    public Stream<org.gridsuite.filter.identifierlistfilter.FilterEquipments> exportFilters(List<UUID> filtersUuids, Network network) {
        return FilterServiceUtils.getFilterEquipmentsFromUuid(network, filtersUuids, this::getFilters).stream();
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
        Map<UUID, Filter> filters = restTemplate
                .exchange(filterServerBaseUri + path, HttpMethod.GET, null, STANDALONE_FILTERS_BY_ID)
                .getBody();
        return filters == null ? Map.of() : filters;
    }

    public Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Map<UUID, String> filters) {
        return exportFilters(new ArrayList<>(filters.keySet()), network)
            .map(f -> new FilterEquipments(f.getFilterId(), filters.get(f.getFilterId()),
                f.getIdentifiableAttributes().stream().map(i -> new IdentifiableAttributes(i.getId(), i.getType(), i.getDistributionKey())).toList(),
                f.getNotFoundEquipments()))
            .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));
    }
}
