/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.filter.utils.EquipmentType;
import org.gridsuite.filter.wip.Filter;
import org.gridsuite.filter.wip.IdentifierListFilter;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * @author Achour BERRAHMA <achour.berrahma at rte-france.com>
 */
class FilterLoaderServiceTest {

    @Test
    void loadDelegatesToTheFilterServerClient() {
        FilterService filterService = mock(FilterService.class);
        UUID filterId = UUID.randomUUID();
        List<UUID> filterUuids = List.of(filterId);
        Map<UUID, Filter> filters = Map.of(filterId, IdentifierListFilter.builder()
                .equipmentType(EquipmentType.GENERATOR).equipmentIds(Set.of("GEN1")).build());
        when(filterService.getStandaloneFilters(filterUuids)).thenReturn(filters);

        Map<UUID, Filter> loadedFilters = new FilterLoaderService(filterService).load(filterUuids);

        assertThat(loadedFilters).isEqualTo(filters);
        verify(filterService).getStandaloneFilters(filterUuids);
    }
}
