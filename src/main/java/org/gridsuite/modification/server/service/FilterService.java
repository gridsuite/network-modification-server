package org.gridsuite.modification.server.service;

import org.gridsuite.modification.server.dto.FilterAttributes;
import org.gridsuite.modification.server.dto.FilterEquipments;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.util.UriComponentsBuilder;

import java.util.List;
import java.util.UUID;

@Service
public class FilterService {
    private static final String FILTER_SERVER_API_VERSION = "v1";

    private static final String DELIMITER = "/";

    private static String filterServerBaseUri;

    private RestTemplate restTemplate;

    @Autowired
    public FilterService(@Value("http://localhost:5027") String filterServerBaseUri) {
        FilterService.filterServerBaseUri = filterServerBaseUri;
        restTemplate = new RestTemplate();
    }

    public void setFilterServerBaseUri(String filterServerBaseUri) {
        FilterService.filterServerBaseUri = filterServerBaseUri;
    }

    public List<FilterAttributes> getFilters(List<String> filtersUuids) {
        var ids = filtersUuids != null && filtersUuids.size() > 0 ?
                "?ids=" + String.join(",", filtersUuids) : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/data" + ids)
                .buildAndExpand()
                .toUriString();
        return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<FilterAttributes>>() {
        }).getBody();
    }

    public List<FilterEquipments> exportFilters(List<String> filtersUuids, UUID networkUuid, String variantId) {
        var ids = filtersUuids.size() > 0 ?
                "&ids="  + String.join(",", filtersUuids) : "";
        var variant = variantId != null ? "&variantId=" + variantId : "";
        String path = UriComponentsBuilder.fromPath(DELIMITER + FILTER_SERVER_API_VERSION + "/filters/export?networkUuid=" + networkUuid + variant + ids)
                .buildAndExpand()
                .toUriString();
        return restTemplate.exchange(filterServerBaseUri + path, HttpMethod.GET, null, new ParameterizedTypeReference<List<FilterEquipments>>() { })
                .getBody();
    }
}
