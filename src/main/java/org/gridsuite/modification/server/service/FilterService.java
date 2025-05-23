/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.iidm.network.Network;
import org.gridsuite.filter.AbstractFilter;
import org.gridsuite.filter.utils.FilterServiceUtils;
import org.gridsuite.modification.IFilterService;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.FilterEquipments;
import org.gridsuite.modification.dto.IdentifiableAttributes;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.gridsuite.modification.NetworkModificationException.Type.FILTERS_NOT_FOUND;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Service
public class FilterService implements IFilterService {
    private static final Logger LOGGER = LoggerFactory.getLogger(FilterService.class);

    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private static String filterServerBaseUri;

    private final RestTemplate restTemplate = new RestTemplate();

    public FilterService(@Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}") String filterServerBaseUri) {
        setFilterServerBaseUri(filterServerBaseUri);
    }

    public static void setFilterServerBaseUri(String filterServerBaseUri) {
        FilterService.filterServerBaseUri = filterServerBaseUri;
    }

    public List<AbstractFilter> getFilters(List<UUID> filtersUuids) {
        var ids = !filtersUuids.isEmpty() ? "?ids=" + filtersUuids.stream().map(UUID::toString).collect(Collectors.joining(",")) : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/metadata" + ids)
            .buildAndExpand()
            .toUriString();
        try {
            return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<AbstractFilter>>() { }).getBody();
        } catch (HttpStatusCodeException e) {
            throw handleChangeError(e, FILTERS_NOT_FOUND);
        }
    }

    public Stream<org.gridsuite.filter.identifierlistfilter.FilterEquipments> exportFilters(List<UUID> filtersUuids, Network network) {
        return FilterServiceUtils.getFilterEquipmentsFromUuid(network, filtersUuids, this::getFilters).stream();
    }

    public Map<UUID, FilterEquipments> getUuidFilterEquipmentsMap(Network network, Map<UUID, String> filters) {
        return exportFilters(new ArrayList<>(filters.keySet()), network)
            .map(f -> new FilterEquipments(f.getFilterId(), filters.get(f.getFilterId()),
                f.getIdentifiableAttributes().stream().map(i -> new IdentifiableAttributes(i.getId(), i.getType(), i.getDistributionKey())).toList(),
                f.getNotFoundEquipments()))
            .collect(Collectors.toMap(FilterEquipments::getFilterId, Function.identity()));
    }

    private NetworkModificationException handleChangeError(HttpStatusCodeException httpException, NetworkModificationException.Type type) {
        String responseBody = httpException.getResponseBodyAsString();
        if (responseBody.isEmpty()) {
            return new NetworkModificationException(type, httpException.getStatusCode().toString());
        }

        String message = responseBody;
        try {
            JsonNode node = new ObjectMapper().readTree(responseBody).path("message");
            if (!node.isMissingNode()) {
                message = node.asText();
            }
        } catch (JsonProcessingException e) {
            // responseBody by default
        }

        LOGGER.error(message, httpException);

        return new NetworkModificationException(type, message);
    }
}
