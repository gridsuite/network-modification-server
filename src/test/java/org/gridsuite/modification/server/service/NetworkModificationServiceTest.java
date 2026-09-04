/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import com.powsybl.network.store.client.NetworkStoreService;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NetworkModificationService.MODIFICATION_LIST_SIZE_MISMATCH_ERROR;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@SpringBootTest
class NetworkModificationServiceTest {

    @Autowired
    private NetworkModificationService networkModificationService;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @Autowired
    private ModificationRepository modificationRepository;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    @Test
    void shouldMapUuidsFromTwoModificationsLists() {
        UUID sourceModificationUuid1 = UUID.randomUUID();
        UUID sourceModificationUuid2 = UUID.randomUUID();
        UUID duplicatedModificationUuid1 = UUID.randomUUID();
        UUID duplicatedModificationUuid2 = UUID.randomUUID();

        List<ModificationInfos> sourceModifications = List.of(
                dummyModification(sourceModificationUuid1),
                dummyModification(sourceModificationUuid2)
        );
        List<ModificationInfos> duplicatedModifications = List.of(
                dummyModification(duplicatedModificationUuid1),
                dummyModification(duplicatedModificationUuid2)
        );

        Map<UUID, UUID> modificationsMapping = new HashMap<>();

        networkModificationService.mapUuidsFromTwoModificationsLists(sourceModifications, duplicatedModifications, modificationsMapping);

        assertEquals(2, modificationsMapping.size());
        assertEquals(duplicatedModificationUuid1, modificationsMapping.get(sourceModificationUuid1));
        assertEquals(duplicatedModificationUuid2, modificationsMapping.get(sourceModificationUuid2));
    }

    @Test
    void shouldRecursivelyMapUuidsFromCompositeModifications() {
        UUID sourceCompositeUuid = UUID.randomUUID();
        UUID sourceChildUuid1 = UUID.randomUUID();
        UUID sourceChildUuid2 = UUID.randomUUID();

        UUID duplicatedCompositeUuid = UUID.randomUUID();
        UUID duplicatedChildUuid1 = UUID.randomUUID();
        UUID duplicatedChildUuid2 = UUID.randomUUID();

        CompositeModificationInfos sourceComposite = compositeModification(
                sourceCompositeUuid,
                List.of(
                        dummyModification(sourceChildUuid1),
                        dummyModification(sourceChildUuid2)
                )
        );

        CompositeModificationInfos duplicatedComposite = compositeModification(
                duplicatedCompositeUuid,
                List.of(
                        dummyModification(duplicatedChildUuid1),
                        dummyModification(duplicatedChildUuid2)
                )
        );

        Map<UUID, UUID> modificationsMapping = new HashMap<>();

        networkModificationService.mapUuidsFromTwoModificationsLists(
                List.of(sourceComposite),
                List.of(duplicatedComposite),
                modificationsMapping
        );

        assertEquals(3, modificationsMapping.size());
        assertEquals(duplicatedCompositeUuid, modificationsMapping.get(sourceCompositeUuid));
        assertEquals(duplicatedChildUuid1, modificationsMapping.get(sourceChildUuid1));
        assertEquals(duplicatedChildUuid2, modificationsMapping.get(sourceChildUuid2));
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenRootListsHaveDifferentSizes() {
        List<ModificationInfos> sourceModifications = List.of(
                dummyModification(UUID.randomUUID()),
                dummyModification(UUID.randomUUID())
        );
        List<ModificationInfos> duplicatedModifications = List.of(
                dummyModification(UUID.randomUUID())
        );

        Map<UUID, UUID> modificationsMapping = new HashMap<>();
        IllegalArgumentException exception = assertThrowsExactly(
                IllegalArgumentException.class,
                () -> networkModificationService.mapUuidsFromTwoModificationsLists(
                        sourceModifications,
                        duplicatedModifications,
                        modificationsMapping
                )
        );
        assertEquals(MODIFICATION_LIST_SIZE_MISMATCH_ERROR, exception.getMessage());
    }

    @Test
    void shouldThrowIllegalArgumentExceptionWhenCompositeChildrenListsHaveDifferentSizes() {
        UUID sourceCompositeUuid = UUID.randomUUID();
        UUID duplicatedCompositeUuid = UUID.randomUUID();

        CompositeModificationInfos sourceComposite = compositeModification(
                sourceCompositeUuid,
                List.of(
                        dummyModification(UUID.randomUUID()),
                        dummyModification(UUID.randomUUID())
                )
        );

        CompositeModificationInfos duplicatedComposite = compositeModification(
                duplicatedCompositeUuid,
                List.of(dummyModification(UUID.randomUUID()))
        );

        Map<UUID, UUID> modificationsMapping = new HashMap<>();
        IllegalArgumentException exception = assertThrowsExactly(
                IllegalArgumentException.class,
                () -> networkModificationService.mapUuidsFromTwoModificationsLists(
                        List.of(sourceComposite),
                        List.of(duplicatedComposite),
                        modificationsMapping
                )
        );
        assertEquals(MODIFICATION_LIST_SIZE_MISMATCH_ERROR, exception.getMessage());
    }

    @Test
    void shouldMoveModificationOutOfCompositeContainerWhenStashed() {
        UUID groupUuid = UUID.randomUUID();
        CompositeModificationInfos compositeModificationInfos = compositeModification(
                UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID()))
        );

        List<ModificationInfos> saved = networkModificationRepository.saveModifications(
                groupUuid, List.of(ModificationEntity.fromDTO(compositeModificationInfos)));
        CompositeModificationInfos savedComposite = (CompositeModificationInfos) saved.get(0);
        UUID compositeUuid = savedComposite.getUuid();
        UUID childUuid = savedComposite.getModificationsInfos().get(0).getUuid();

        // sanity check: the modification is nested inside the composite before stashing
        assertEquals(compositeUuid, modificationRepository.findCompositeContainerIdByModificationId(childUuid));

        networkModificationService.stashNetworkModifications(groupUuid, List.of(childUuid));

        // the modification was moved out of the composite, into the group, before being stashed
        assertNull(modificationRepository.findCompositeContainerIdByModificationId(childUuid));
        assertEquals(1, networkModificationRepository.getModificationsCount(groupUuid, true));
        assertTrue(networkModificationRepository.getModificationsMetadata(groupUuid, true).stream()
                .anyMatch(modificationInfos -> modificationInfos.getUuid().equals(childUuid)));
        // the composite itself is left in place, now empty
        assertEquals(List.of(compositeUuid), modificationRepository.findAllChildrenUuids(compositeUuid));
    }

    @Test
    void shouldFindParentCompositeOnlyForModificationsNestedInAComposite() {
        UUID groupUuid = UUID.randomUUID();
        CompositeModificationInfos compositeModificationInfos = compositeModification(
                UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID()))
        );

        List<ModificationInfos> saved = networkModificationRepository.saveModifications(
                groupUuid, List.of(
                        ModificationEntity.fromDTO(compositeModificationInfos),
                        ModificationEntity.fromDTO(dummyModification(UUID.randomUUID()))
                ));
        CompositeModificationInfos savedComposite = (CompositeModificationInfos) saved.get(0);
        UUID compositeUuid = savedComposite.getUuid();
        UUID nestedChildUuid = savedComposite.getModificationsInfos().get(0).getUuid();
        UUID directGroupChildUuid = saved.get(1).getUuid();

        Map<UUID, UUID> parentComposites = networkModificationService.findModificationParentComposites(
                List.of(nestedChildUuid, directGroupChildUuid));

        assertEquals(1, parentComposites.size());
        assertEquals(compositeUuid, parentComposites.get(nestedChildUuid));
        assertFalse(parentComposites.containsKey(directGroupChildUuid));
    }

    @Test
    void shouldNotApplyModificationsWhenNetworkDoesNotExist() {
        UUID networkUuid = UUID.randomUUID();
        UUID targetGroupUuid = UUID.randomUUID();
        List<ModificationInfos> saved = networkModificationRepository.saveModifications(
                UUID.randomUUID(), List.of(ModificationEntity.fromDTO(dummyModification(UUID.randomUUID()))));

        when(networkStoreService.networkExists(any(UUID.class))).thenReturn(false);

        NetworkModificationsResult result = networkModificationService.duplicateModifications(
                targetGroupUuid,
                null,
                saved.stream().map(ModificationInfos::getUuid).toList(),
                List.of(new ModificationApplicationContext(networkUuid, "variant", UUID.randomUUID(), UUID.randomUUID(), Set.of()))
        ).join();

        assertEquals(1, result.modificationUuids().size());
        assertEquals(1, result.modificationResults().size());
        assertTrue(result.modificationResults().get(0).isEmpty());
    }

    private static LoadModificationInfos dummyModification(UUID uuid) {
        return LoadModificationInfos.builder()
                .equipmentId("dummyEquipmentId")
                .uuid(uuid)
                .build();
    }

    private static CompositeModificationInfos compositeModification(UUID uuid, List<ModificationInfos> children) {
        return CompositeModificationInfos.builder()
                .uuid(uuid)
                .modificationsInfos(children)
                .build();
    }
}
