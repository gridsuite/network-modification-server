/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.LoadCreationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.dto.PermissionType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.web.client.RestClientException;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * @author Florent MILLOT <florent.millot at rte-france.com>
 */
class ModificationPermissionServiceTest {
    private static final String USER_ID = "userId";

    private DirectoryService directoryService;
    private ModificationPermissionService modificationPermissionService;

    @BeforeEach
    void setUp() {
        directoryService = mock(DirectoryService.class);
        modificationPermissionService = new ModificationPermissionService(directoryService);
    }

    @Test
    void aPayloadWithoutReferenceAsksNothing() {
        List<ModificationInfos> modifications = List.of(loadCreation(), composite(loadCreation()));

        modificationPermissionService.withPermissions(modifications, USER_ID);

        verifyNoInteractions(directoryService);
    }

    @Test
    void aPayloadReadWithoutUserAsksNothing() {
        ModificationReferenceInfos reference = reference(UUID.randomUUID(), null);

        modificationPermissionService.withPermissions(List.of(reference), null);

        verifyNoInteractions(directoryService);
        assertThat(reference.getPermission()).isNull();
    }

    @Test
    void theSameSharedModificationIsAskedOnlyOnce() {
        UUID sharedUuid = UUID.randomUUID();
        ModificationReferenceInfos firstReference = reference(sharedUuid, null);
        ModificationReferenceInfos secondReference = reference(sharedUuid, null);
        when(directoryService.getElementsPermissions(any(), eq(USER_ID))).thenReturn(Map.of(sharedUuid, PermissionType.WRITE));

        modificationPermissionService.withPermissions(List.of(firstReference, secondReference), USER_ID);

        ArgumentCaptor<Collection<UUID>> askedUuids = ArgumentCaptor.captor();
        verify(directoryService, times(1)).getElementsPermissions(askedUuids.capture(), eq(USER_ID));
        assertThat(askedUuids.getValue()).containsExactly(sharedUuid);
        assertThat(firstReference.getPermission()).isEqualTo(PermissionType.WRITE);
        assertThat(secondReference.getPermission()).isEqualTo(PermissionType.WRITE);
    }

    @Test
    void aNestedReferenceGetsItsOwnPermission() {
        UUID outerUuid = UUID.randomUUID();
        UUID nestedUuid = UUID.randomUUID();
        ModificationReferenceInfos nestedReference = reference(nestedUuid, null);
        ModificationReferenceInfos outerReference = reference(outerUuid, composite(loadCreation(), nestedReference));
        when(directoryService.getElementsPermissions(any(), eq(USER_ID)))
                .thenReturn(Map.of(outerUuid, PermissionType.WRITE, nestedUuid, PermissionType.READ));

        modificationPermissionService.withPermissions(outerReference, USER_ID);

        ArgumentCaptor<Collection<UUID>> askedUuids = ArgumentCaptor.captor();
        verify(directoryService, times(1)).getElementsPermissions(askedUuids.capture(), eq(USER_ID));
        assertThat(askedUuids.getValue()).containsExactlyInAnyOrder(outerUuid, nestedUuid);
        assertThat(outerReference.getPermission()).isEqualTo(PermissionType.WRITE);
        assertThat(nestedReference.getPermission()).isEqualTo(PermissionType.READ);
    }

    @Test
    void anElementTheDirectoryLeavesOutHoldsNoPermission() {
        UUID knownUuid = UUID.randomUUID();
        ModificationReferenceInfos knownReference = reference(knownUuid, null);
        ModificationReferenceInfos unknownReference = reference(UUID.randomUUID(), null);
        when(directoryService.getElementsPermissions(any(), eq(USER_ID))).thenReturn(Map.of(knownUuid, PermissionType.MANAGE));

        modificationPermissionService.withPermissions(List.of(knownReference, unknownReference), USER_ID);

        verify(directoryService, times(1)).getElementsPermissions(any(), eq(USER_ID));
        assertThat(knownReference.getPermission()).isEqualTo(PermissionType.MANAGE);
        assertThat(unknownReference.getPermission()).isEqualTo(PermissionType.NONE);
    }

    @Test
    void anUnreachableDirectoryLeavesThePermissionUnresolved() {
        ModificationReferenceInfos reference = reference(UUID.randomUUID(), null);
        when(directoryService.getElementsPermissions(any(), eq(USER_ID))).thenThrow(new RestClientException("directory-server is down"));

        modificationPermissionService.withPermissions(List.of(reference), USER_ID);

        assertThat(reference.getPermission()).isNull();
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
