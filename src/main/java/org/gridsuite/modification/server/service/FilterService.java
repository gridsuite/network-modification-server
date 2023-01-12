/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.FilterEquipments;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
public class FilterService {
    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    @Value("${gridsuite.services.filter-server.base-uri:http://filter-server/}")
    private String filterServerBaseUri;

    private final RestTemplate restTemplate;

    public FilterService() {
        restTemplate = new RestTemplate();
    }

    public void setFilterServerBaseUri(String filterServerBaseUri) {
        this.filterServerBaseUri = filterServerBaseUri;
    }

    public List<FilterEquipments> exportFilters(List<UUID> filtersUuids, UUID networkUuid, String variantId) {
        var ids = !filtersUuids.isEmpty() ?
                "&ids="  + filtersUuids.stream().map(UUID::toString).collect(Collectors.joining(",")) : "";
        var variant = variantId != null ? "&variantId=" + variantId : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/export?networkUuid=" + networkUuid + variant + ids)
                .buildAndExpand()
                .toUriString();
        return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<FilterEquipments>>() { })
                .getBody();
    }
}
