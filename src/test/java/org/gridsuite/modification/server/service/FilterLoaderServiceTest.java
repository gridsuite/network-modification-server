/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.utils.expertfilter.CombinatorType;
import org.gridsuite.filter.wip.ExpertFilter;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.filter.wip.rule.CombinatorExpertRule;
import org.gridsuite.modification.server.RestTemplateConfig;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.HttpMethod;
import org.springframework.test.web.client.MockRestServiceServer;
import org.springframework.web.client.RestTemplate;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.method;
import static org.springframework.test.web.client.match.MockRestRequestMatchers.requestTo;
import static org.springframework.test.web.client.response.MockRestResponseCreators.withSuccess;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class FilterLoaderServiceTest {

    private static final String FILTER_SERVER_BASE_URI = "http://filter-server-test";
    private static final String STANDALONE_FILTERS_PATH = "/v1/standalone-filters";

    private final RestTemplateConfig restTemplateConfig = new RestTemplateConfig();
    private final ObjectMapper objectMapper = restTemplateConfig.objectMapper();

    private MockRestServiceServer filterServer;
    private FilterLoaderService filterLoaderService;

    @BeforeEach
    void setUp() {
        RestTemplate restTemplate = restTemplateConfig.restTemplate(new RestTemplateBuilder());
        filterServer = MockRestServiceServer.createServer(restTemplate);
        filterLoaderService = new FilterLoaderService(FILTER_SERVER_BASE_URI, restTemplate);
    }

    @Test
    void loadShouldQueryTheFilterServerAndReturnFiltersIndexedById() throws JsonProcessingException {
        // Arrange
        UUID identifierListFilterId = UUID.randomUUID();
        UUID expertFilterId = UUID.randomUUID();
        Map<UUID, Filter> filtersById = new LinkedHashMap<>();
        filtersById.put(identifierListFilterId, IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR).equipmentIds(Set.of("GEN1", "GEN2")).build());
        filtersById.put(expertFilterId, ExpertFilter.builder()
                .equipmentType(EquipmentType.LINE)
                .rule(CombinatorExpertRule.builder().combinator(CombinatorType.AND).rules(List.of()).build())
                .build());
        expectStandaloneFiltersRequest(filtersById, identifierListFilterId, expertFilterId);

        // Act
        Map<UUID, Filter> loadedFilters = filterLoaderService.load(List.of(identifierListFilterId, expertFilterId));

        // Assert : the polymorphic payload is deserialized back into self-contained filters
        assertThat(loadedFilters).hasSize(2)
                .hasEntrySatisfying(identifierListFilterId, filter -> {
                    assertThat(filter).isInstanceOf(IdentifierListFilter.class);
                    assertThat(filter.getEquipmentType()).isEqualTo(EquipmentType.GENERATOR);
                    assertThat(((IdentifierListFilter) filter).getEquipmentIds()).containsExactlyInAnyOrder("GEN1", "GEN2");
                })
                .hasEntrySatisfying(expertFilterId, filter -> {
                    assertThat(filter).isInstanceOf(ExpertFilter.class);
                    assertThat(filter.getEquipmentType()).isEqualTo(EquipmentType.LINE);
                });
        filterServer.verify();
    }

    @Test
    void loadShouldOmitFiltersThatDoNotExistAnymore() throws JsonProcessingException {
        // Arrange : the filter server silently omits ids it cannot resolve
        UUID existingFilterId = UUID.randomUUID();
        UUID deletedFilterId = UUID.randomUUID();
        Map<UUID, Filter> filtersById = Map.of(existingFilterId, IdentifierListFilter.builder()
                .equipmentType(EquipmentType.LOAD).equipmentIds(Set.of("LOAD1")).build());
        expectStandaloneFiltersRequest(filtersById, existingFilterId, deletedFilterId);

        // Act
        Map<UUID, Filter> loadedFilters = filterLoaderService.load(List.of(existingFilterId, deletedFilterId));

        // Assert : callers detect deleted filters by diffing the requested ids with the returned keys
        assertThat(loadedFilters).containsOnlyKeys(existingFilterId).doesNotContainValue(null);
        filterServer.verify();
    }

    @Test
    void loadShouldReturnAnEmptyMapWhenNoFilterIsResolved() throws JsonProcessingException {
        // Arrange
        UUID deletedFilterId = UUID.randomUUID();
        expectStandaloneFiltersRequest(Map.of(), deletedFilterId);

        // Act & Assert
        assertThat(filterLoaderService.load(List.of(deletedFilterId))).isEmpty();
        filterServer.verify();
    }

    @Test
    void loadShouldNotCallTheFilterServerWhenNoFilterIsRequested() {
        // Act & Assert : no expectation is registered, so any outgoing request would fail the test
        assertThat(filterLoaderService.load(List.of())).isEmpty();
        assertThat(filterLoaderService.load(null)).isEmpty();
        filterServer.verify();
    }

    private void expectStandaloneFiltersRequest(Map<UUID, Filter> responseBody, UUID... requestedIds) throws JsonProcessingException {
        StringBuilder expectedUri = new StringBuilder(FILTER_SERVER_BASE_URI).append(STANDALONE_FILTERS_PATH);
        for (int i = 0; i < requestedIds.length; i++) {
            expectedUri.append(i == 0 ? '?' : '&').append("ids=").append(requestedIds[i]);
        }
        filterServer.expect(requestTo(expectedUri.toString()))
                .andExpect(method(HttpMethod.GET))
                .andRespond(withSuccess(objectMapper.writeValueAsString(responseBody), APPLICATION_JSON));
    }
}
