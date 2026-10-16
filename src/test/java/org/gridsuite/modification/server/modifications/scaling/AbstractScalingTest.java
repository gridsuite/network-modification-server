/*
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.modifications.scaling;

import com.github.tomakehurst.wiremock.client.WireMock;
import lombok.SneakyThrows;
import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.gridsuite.modification.context.dto.FilterWithDistributionKeys;
import org.gridsuite.modification.server.modifications.AbstractNetworkModificationTest;
import org.gridsuite.modification.server.utils.FilterWithDistributionKeysStub;
import org.gridsuite.modification.server.utils.StubbedFilterRequest;
import org.gridsuite.modification.server.utils.WireMockUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author Kamil MARUT <kamil.marut at rte-france.com>
 */
public abstract class AbstractScalingTest extends AbstractNetworkModificationTest {

    protected static final String PATH = "/v1/standalone-filters/with-distribution-keys";

    protected static final String IDS_PARAM = "ids";

    /**
     * The filters of the whole scaling, as served by the filter server: every filter of a given
     * scaling is resolved in a single call, so they are all stubbed together.
     */
    protected abstract List<FilterWithDistributionKeysStub> getTestFilters();

    protected FilterWithDistributionKeysStub createFilterStub(EquipmentType equipmentType, UUID filterId, Map<String, Double> distributionKeys) {
        return new FilterWithDistributionKeysStub(filterId,
                IdentifierListFilter.builder()
                        .equipmentType(equipmentType)
                        .equipmentIds(distributionKeys.keySet())
                        .build(),
                distributionKeys);
    }

    /**
     * Stubs one filter server request per distinct set of filter identifiers of the given requests, and
     * keeps the occurrences of each of them, so that a single call to
     * {@link #verifyFiltersWithDistributionKeysRequests(List)} verifies all of them.
     */
    protected List<StubbedFilterRequest> stubScalingFilters(List<List<UUID>> filterUuidsPerRequest) {
        Map<UUID, FilterWithDistributionKeysStub> filterStubsById = getTestFilters().stream()
                .collect(Collectors.toMap(FilterWithDistributionKeysStub::id, Function.identity()));
        Map<Set<UUID>, Integer> requestCounts = new LinkedHashMap<>();
        filterUuidsPerRequest.forEach(filterUuids -> requestCounts.merge(Set.copyOf(filterUuids), 1, Integer::sum));

        List<StubbedFilterRequest> stubbedFilterRequests = new ArrayList<>();
        for (Map.Entry<Set<UUID>, Integer> requestCount : requestCounts.entrySet()) {
            List<FilterWithDistributionKeysStub> filterStubs = requestCount.getKey().stream()
                    .map(filterId -> Objects.requireNonNull(filterStubsById.get(filterId), "No filter stub declared for " + filterId))
                    .toList();
            stubbedFilterRequests.add(new StubbedFilterRequest(stubFiltersWithDistributionKeys(filterStubs), requestCount.getKey(), requestCount.getValue()));
        }
        return stubbedFilterRequests;
    }

    @SneakyThrows
    protected UUID stubFiltersWithDistributionKeys(List<FilterWithDistributionKeysStub> filterStubs) {
        List<UUID> filterIds = filterStubs.stream().map(FilterWithDistributionKeysStub::id).toList();
        Map<UUID, FilterWithDistributionKeys> filtersWithDistributionKeys = filterStubs.stream()
                .collect(Collectors.toMap(FilterWithDistributionKeysStub::id, stub -> FilterWithDistributionKeys.builder()
                        .filter(stub.filter())
                        .distributionKeys(stub.distributionKeys())
                        .build()));
        return wireMockServer.stubFor(WireMock.get(WireMock.urlPathEqualTo(PATH))
                .withQueryParam(IDS_PARAM, WireMockUtils.havingExactlyIdsIgnoringOrder(filterIds))
                .willReturn(WireMock.ok()
                        .withBody(mapper.writeValueAsString(filtersWithDistributionKeys))
                        .withHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE))).getId();
    }

    protected void verifyFiltersWithDistributionKeysRequest(UUID stubId, Collection<UUID> filterIds) {
        verifyFiltersWithDistributionKeysRequest(stubId, filterIds, 1);
    }

    protected void verifyFiltersWithDistributionKeysRequest(UUID stubId, Collection<UUID> filterIds, int nbRequests) {
        wireMockUtils.verifyGetRequest(stubId, PATH, IDS_PARAM, WireMockUtils.havingExactlyIdsIgnoringOrder(filterIds), false, nbRequests);
    }

    protected void verifyFiltersWithDistributionKeysRequests(List<StubbedFilterRequest> stubs) {
        stubs.forEach(stub -> verifyFiltersWithDistributionKeysRequest(stub.stubId(), stub.filterIds(), stub.requestCount()));
    }
}
