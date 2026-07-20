package org.gridsuite.modification.server.service;

import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.LoadModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.gridsuite.modification.server.service.NetworkModificationService.DIFFERENT_SIZES_ERROR_MESSAGE;
import static org.gridsuite.modification.server.service.NetworkModificationService.mapUuidsFromTwoModificationsLists;
import static org.junit.jupiter.api.Assertions.*;

class NetworkModificationServiceTest {
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

        mapUuidsFromTwoModificationsLists(sourceModifications, duplicatedModifications, modificationsMapping);

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

        mapUuidsFromTwoModificationsLists(
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
                () -> mapUuidsFromTwoModificationsLists(
                        sourceModifications,
                        duplicatedModifications,
                        modificationsMapping
                )
        );
        assertEquals(DIFFERENT_SIZES_ERROR_MESSAGE, exception.getMessage());
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
                () -> mapUuidsFromTwoModificationsLists(
                        List.of(sourceComposite),
                        List.of(duplicatedComposite),
                        modificationsMapping
                )
        );
        assertEquals(DIFFERENT_SIZES_ERROR_MESSAGE, exception.getMessage());
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
