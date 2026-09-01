/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.byfilter;

import com.github.tomakehurst.wiremock.client.WireMock;
import com.github.tomakehurst.wiremock.matching.MultiValuePattern;
import com.powsybl.iidm.network.IdentifiableType;
import lombok.SneakyThrows;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.FilterStub;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author Joris Mancini <joris.mancini_externe at rte-france.com>
 */
public abstract class AbstractByFilterTest extends AbstractNetworkModificationTest {

    protected static final String PATH = "/v1/standalone-filters";

    protected abstract Map<UUID, Set<String>> getFilterMapping();

    protected abstract IdentifiableType getIdentifiableType();

    protected abstract EquipmentType getEquipmentType();

    protected FilterStub createFilterStub(UUID filterID, Collection<String> equipmentIds) {
        return new FilterStub(filterID, equipmentFilter(Set.copyOf(equipmentIds)));
    }

    protected Filter equipmentFilter(Set<String> equipmentIds) {
        return IdentifierListFilter.builder()
                .equipmentType(getEquipmentType())
                .equipmentIds(equipmentIds)
                .build();
    }

    protected List<StubbedFilterRequest> stubStandaloneFilterRequests(List<List<UUID>> filterIdsList) {
        Map<Set<UUID>, Integer> requestCounts = new LinkedHashMap<>();
        filterIdsList.forEach(filterIds -> requestCounts.merge(filterIds.stream().collect(Collectors.toSet()), 1, Integer::sum));

        Map<UUID, Filter> filtersById = getFilterMapping().entrySet().stream()
                .collect(Collectors.toMap(Map.Entry::getKey, entry -> equipmentFilter(entry.getValue())));

        List<StubbedFilterRequest> stubbedFilterRequests = new ArrayList<>();
        for (Map.Entry<Set<UUID>, Integer> requestCount : requestCounts.entrySet()) {
            List<FilterStub> filterStubs = requestCount.getKey().stream()
                    .map(filterId -> new FilterStub(filterId, Objects.requireNonNull(filtersById.get(filterId))))
                    .toList();
            stubbedFilterRequests.add(new StubbedFilterRequest(stubStandaloneFilters(filterStubs), requestCount.getKey(), requestCount.getValue()));
        }
        return stubbedFilterRequests;
    }

    @SneakyThrows
    protected UUID stubStandaloneFilters(List<FilterStub> filterStubs) {
        List<UUID> filterIds = filterStubs.stream().map(FilterStub::id).toList();
        return wireMockServer.stubFor(WireMock.get(WireMock.urlPathEqualTo(PATH))
                .withQueryParam("ids", havingExactlyIdsIgnoringOrder(filterIds))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filterStubs.stream().map(FilterStub::filter).toList()))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
    }

    protected void verifyStandaloneFiltersRequest(UUID stubId, Set<UUID> filterIds) {
        verifyStandaloneFiltersRequest(stubId, filterIds, 1);
    }

    protected void verifyStandaloneFiltersRequest(UUID stubId, Set<UUID> filterIds, int nbRequests) {
        wireMockUtils.verifyGetRequest(stubId, PATH, "ids", havingExactlyIdsIgnoringOrder(filterIds), false, nbRequests);
    }

    protected void verifyStandaloneFiltersRequests(List<StubbedFilterRequest> stubs) {
        stubs.forEach(stub -> wireMockUtils.verifyGetRequest(
                stub.stubId(), PATH, "ids", havingExactlyIdsIgnoringOrder(stub.filterIds()), false, stub.requestCount()));
    }

    protected MultiValuePattern havingExactlyIdsIgnoringOrder(Collection<UUID> filterIds) {
        String[] expectedIds = filterIds.stream()
                .map(UUID::toString)
                .distinct()
                .toArray(String[]::new);
        return WireMock.havingExactly(expectedIds);
    }
}
