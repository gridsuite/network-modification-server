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
import org.gridsuite.modification.server.NetworkModificationException;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static io.swagger.v3.oas.integration.StringOpenApiConfigurationLoader.LOGGER;
import static org.gridsuite.modification.server.NetworkModificationException.Type.FILTERS_NOT_FOUND;

/**
 * @author bendaamerahm <ahmed.bendaamer at rte-france.com>
 */
@Service
public class FilterService {
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

    public List<FilterEquipments> exportFilters(Set<UUID> filtersUuids, UUID networkUuid, String variantId) {
        var ids = !filtersUuids.isEmpty() ?
                "&ids=" + filtersUuids.stream().map(UUID::toString).collect(Collectors.joining(",")) : "";
        var variant = variantId != null ? "&variantId=" + variantId : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/export?networkUuid=" + networkUuid + variant + ids)
                .buildAndExpand()
                .toUriString();
        try {
            return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<FilterEquipments>>() { })
                    .getBody();
        } catch (HttpStatusCodeException e) {
            throw handleChangeError(e, FILTERS_NOT_FOUND);
        }
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
