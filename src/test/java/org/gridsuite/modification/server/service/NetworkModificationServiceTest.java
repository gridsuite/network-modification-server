/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NetworkModificationService.MODIFICATION_LIST_SIZE_MISMATCH_ERROR;
import static org.junit.jupiter.api.Assertions.*;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@SpringBootTest
class NetworkModificationServiceTest {

    @Autowired
    private NetworkModificationService networkModificationService;

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
