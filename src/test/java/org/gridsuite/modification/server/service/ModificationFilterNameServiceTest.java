/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.ByFilterDeletionInfos;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.FilterInfos;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.web.client.RestClientException;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * @author Hugo Marcellin <hugo.marcelin at rte-france.com>
 */
class ModificationFilterNameServiceTest {
    private DirectoryService directoryService;
    private ModificationFilterNameService modificationFilterNameService;

    @BeforeEach
    void setUp() {
        directoryService = mock(DirectoryService.class);
        modificationFilterNameService = new ModificationFilterNameService(directoryService);
    }

    @Test
    void aPayloadWithoutFilterAsksNothing() {
        List<ModificationInfos> modifications = List.of(loadCreation(), composite(loadCreation()));

        modificationFilterNameService.addFilterNames(modifications);

        verifyNoInteractions(directoryService);
    }

    @Test
    void theSameFilterIsAskedOnlyOnce() {
        UUID filterUuid = UUID.randomUUID();
        FilterInfos firstFilter = filter(filterUuid);
        FilterInfos secondFilter = filter(filterUuid);
        when(directoryService.getElementNames(any())).thenReturn(Map.of(filterUuid, "filter"));

        modificationFilterNameService.addFilterNames(List.of(byFilterDeletion(firstFilter), byFilterDeletion(secondFilter)));

        ArgumentCaptor<Collection<UUID>> askedUuids = ArgumentCaptor.captor();
        verify(directoryService, times(1)).getElementNames(askedUuids.capture());
        assertThat(askedUuids.getValue()).containsExactly(filterUuid);
        assertThat(firstFilter.getName()).isEqualTo("filter");
        assertThat(secondFilter.getName()).isEqualTo("filter");
    }

    @Test
    void aNestedFilterGetsItsName() {
        FilterInfos sharedFilter = filter(UUID.randomUUID());
        FilterInfos nestedFilter = filter(UUID.randomUUID());
        ModificationReferenceInfos reference = reference(UUID.randomUUID(),
                composite(byFilterDeletion(sharedFilter), composite(loadCreation(), byFilterDeletion(nestedFilter))));
        when(directoryService.getElementNames(any()))
                .thenReturn(Map.of(sharedFilter.getId(), "shared filter", nestedFilter.getId(), "nested filter"));

        modificationFilterNameService.addFilterNames(reference);

        ArgumentCaptor<Collection<UUID>> askedUuids = ArgumentCaptor.captor();
        verify(directoryService, times(1)).getElementNames(askedUuids.capture());
        assertThat(askedUuids.getValue()).containsExactlyInAnyOrder(sharedFilter.getId(), nestedFilter.getId());
        assertThat(sharedFilter.getName()).isEqualTo("shared filter");
        assertThat(nestedFilter.getName()).isEqualTo("nested filter");
    }

    @Test
    void aFilterTheDirectoryLeavesOutGetsNoName() {
        FilterInfos renamedFilter = FilterInfos.builder().id(UUID.randomUUID()).name("name when picked").build();
        FilterInfos deletedFilter = FilterInfos.builder().id(UUID.randomUUID()).name("deleted since").build();
        when(directoryService.getElementNames(any())).thenReturn(Map.of(renamedFilter.getId(), "current name"));

        modificationFilterNameService.addFilterNames(byFilterDeletion(renamedFilter, deletedFilter));

        verify(directoryService, times(1)).getElementNames(any());
        assertThat(renamedFilter.getName()).isEqualTo("current name");
        assertThat(deletedFilter.getName()).isNull();
    }

    @Test
    void anUnreachableDirectoryLeavesTheNamesForReportsUnset() {
        FilterInfos filter = filter(UUID.randomUUID());
        when(directoryService.getElementNames(any())).thenThrow(new RestClientException("directory-server is down"));

        modificationFilterNameService.addFilterNamesForReports(List.of(byFilterDeletion(filter)));

        assertThat(filter.getName()).isNull();
    }

    @Test
    void anUnreachableDirectoryFailsTheRead() {
        ModificationInfos modification = byFilterDeletion(filter(UUID.randomUUID()));
        when(directoryService.getElementNames(any())).thenThrow(new RestClientException("directory-server is down"));

        assertThrows(RestClientException.class, () -> modificationFilterNameService.addFilterNames(modification));
    }

    /** A filter as read from the database: its name is not stored */
    private static FilterInfos filter(UUID filterUuid) {
        return FilterInfos.builder().id(filterUuid).build();
    }

    private static ModificationInfos byFilterDeletion(FilterInfos... filters) {
        return ByFilterDeletionInfos.builder()
                .uuid(UUID.randomUUID())
                .filters(List.of(filters))
                .build();
    }

    private static ModificationReferenceInfos reference(UUID referencedId, ModificationInfos referencedInfos) {
        return ModificationReferenceInfos.builder()
                .uuid(UUID.randomUUID())
                .referenceType(ModificationReferenceInfos.Type.DIRECTORY)
                .referencedId(referencedId)
                .referencedInfos(referencedInfos)
                .build();
    }

    private static CompositeModificationInfos composite(ModificationInfos... content) {
        return CompositeModificationInfos.builder()
                .uuid(UUID.randomUUID())
                .modificationsInfos(List.of(content))
                .build();
    }

    private static ModificationInfos loadCreation() {
        return LoadCreationInfos.builder().uuid(UUID.randomUUID()).equipmentId("load").build();
    }
}
