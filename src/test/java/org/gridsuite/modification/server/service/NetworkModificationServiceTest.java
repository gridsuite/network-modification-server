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
import org.gridsuite.modification.dto.ModificationReferenceInfos;
import org.gridsuite.modification.server.ContextConfigurationWithTestChannel;
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.dto.StashedFilter;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.ModificationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@SpringBootTest
@ContextConfigurationWithTestChannel
class NetworkModificationServiceTest {
    private static final String TEST_USER_ID = "userId";

    @Autowired
    private NetworkModificationService networkModificationService;

    @MockitoBean
    private DirectoryService directoryService;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @Autowired
    private ModificationRepository modificationRepository;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    @Test
    void shouldMoveModificationOutOfCompositeContainerWhenStashed() {
        UUID groupUuid = UUID.randomUUID();
        CompositeModificationInfos compositeModificationInfos = compositeModification(
                UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID()))
        );

        List<ModificationInfos> saved = networkModificationRepository.saveModifications(
                groupUuid, List.of(ModificationEntity.fromDTO(compositeModificationInfos)));
        CompositeModificationInfos savedComposite = (CompositeModificationInfos) saved.getFirst();
        UUID compositeUuid = savedComposite.getUuid();
        UUID childUuid = savedComposite.getModificationsInfos().getFirst().getUuid();

        // sanity check: the modification is nested inside the composite before stashing
        assertEquals(compositeUuid, modificationRepository.findCompositeContainerIdByModificationId(childUuid));

        networkModificationService.stashNetworkModifications(groupUuid, List.of(childUuid), TEST_USER_ID);

        // the modification was moved out of the composite, into the group, before being stashed
        assertNull(modificationRepository.findCompositeContainerIdByModificationId(childUuid));
        assertEquals(1, networkModificationRepository.getModificationsCount(groupUuid, true));
        assertTrue(networkModificationRepository.getModifications(groupUuid, true, StashedFilter.STASHED).stream()
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
        CompositeModificationInfos savedComposite = (CompositeModificationInfos) saved.getFirst();
        UUID compositeUuid = savedComposite.getUuid();
        UUID nestedChildUuid = savedComposite.getModificationsInfos().getFirst().getUuid();
        UUID directGroupChildUuid = saved.get(1).getUuid();

        Map<UUID, UUID> parentComposites = networkModificationService.findModificationParentComposites(
                List.of(nestedChildUuid, directGroupChildUuid));

        assertEquals(1, parentComposites.size());
        assertEquals(compositeUuid, parentComposites.get(nestedChildUuid));
        assertFalse(parentComposites.containsKey(directGroupChildUuid));
    }

    @Test
    void shouldFindReferencesNestedInComposites() {
        UUID sharedUuid = saveComposite(UUID.randomUUID(), List.of(dummyModification(UUID.randomUUID())));
        UUID groupWithoutReferenceUuid = UUID.randomUUID();
        saveComposite(groupWithoutReferenceUuid, List.of(dummyModification(UUID.randomUUID())));
        // outer ── inner ── reference to the shared composite
        UUID groupWithReferenceUuid = UUID.randomUUID();
        UUID outerUuid = saveComposite(groupWithReferenceUuid,
                List.of(compositeModification(UUID.randomUUID(), List.of(referenceTo(sharedUuid)))));

        assertFalse(networkModificationService.hasModificationReferences(List.of()), "no container, nothing to look into");
        assertFalse(networkModificationService.hasModificationReferences(List.of(groupWithoutReferenceUuid)));
        assertTrue(networkModificationService.hasModificationReferences(List.of(groupWithReferenceUuid)), "a reference nested two composites deep");
        assertTrue(networkModificationService.hasModificationReferences(List.of(outerUuid)), "a composite is a container too");
        assertTrue(networkModificationService.hasModificationReferences(List.of(groupWithoutReferenceUuid, groupWithReferenceUuid)),
                "any of the containers holding one is enough");
    }

    @Test
    void shouldGetContainersWithReferencesNestedInComposites() {
        UUID sharedUuid = saveComposite(UUID.randomUUID(), List.of(dummyModification(UUID.randomUUID())));
        UUID groupWithoutReferenceUuid = UUID.randomUUID();
        saveComposite(groupWithoutReferenceUuid, List.of(dummyModification(UUID.randomUUID())));
        // outer ── inner ── reference to the shared composite
        UUID groupWithReferenceUuid = UUID.randomUUID();
        UUID outerUuid = saveComposite(groupWithReferenceUuid,
                List.of(compositeModification(UUID.randomUUID(), List.of(referenceTo(sharedUuid)))));
        // the only reference of this group is in the bin
        UUID groupWithStashedReferenceUuid = UUID.randomUUID();
        networkModificationRepository.saveModifications(groupWithStashedReferenceUuid, List.of(ModificationEntity.fromDTO(
                CompositeModificationInfos.builder()
                        .uuid(UUID.randomUUID())
                        .modificationsInfos(List.of(referenceTo(sharedUuid)))
                        .stashed(true)
                        .build())));

        assertTrue(networkModificationService.getContainersWithModificationReferences(List.of()).isEmpty(), "no container, nothing to look into");
        assertEquals(List.of(groupWithReferenceUuid), networkModificationService.getContainersWithModificationReferences(
                List.of(groupWithoutReferenceUuid, groupWithReferenceUuid, groupWithStashedReferenceUuid)),
                "a reference nested two composites deep, a stashed one does not count");
        assertEquals(List.of(outerUuid), networkModificationService.getContainersWithModificationReferences(List.of(outerUuid)),
                "a composite is a container too");
    }

    /**
     * @return the uuid of a composite holding the given content, saved in the given group
     */
    private UUID saveComposite(UUID groupUuid, List<ModificationInfos> children) {
        return networkModificationRepository.saveModifications(groupUuid,
                List.of(ModificationEntity.fromDTO(compositeModification(UUID.randomUUID(), children)))).getFirst().getUuid();
    }

    private static ModificationReferenceInfos referenceTo(UUID sharedCompositeUuid) {
        return ModificationReferenceInfos.builder()
                .referencedId(sharedCompositeUuid)
                .referenceType(ModificationReferenceInfos.Type.COMPOSITE)
                .referencedInfos(CompositeModificationInfos.builder().uuid(sharedCompositeUuid).build())
                .build();
    }

    private static LoadModificationInfos dummyModification(UUID uuid) {
        return LoadModificationInfos.builder()
                .equipmentId("dummyEquipmentId")
                .uuid(uuid)
                .build();
    }

    private static LoadModificationInfos dummyModification(UUID uuid, String equipmentId) {
        return LoadModificationInfos.builder()
                .equipmentId(equipmentId)
                .uuid(uuid)
                .build();
    }

    private static CompositeModificationInfos compositeModification(UUID uuid, List<ModificationInfos> children) {
        return CompositeModificationInfos.builder()
                .uuid(uuid)
                .modificationsInfos(children)
                .build();
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
                List.of(new ModificationApplicationContext(networkUuid, "variant", UUID.randomUUID(), UUID.randomUUID(), "tag1"))
        ).join();

        assertEquals(1, result.modificationUuids().size());
        assertEquals(1, result.modificationResults().size());
        assertTrue(result.modificationResults().getFirst().isEmpty());
    }

    @Test
    void shouldUnshareReferenceModificationAndReplaceByCopy() {
        // 1. Create a shared composite modification
        UUID sharedCompositeUuid = saveComposite(UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID())));

        // 2. Create a group with a reference to the shared composite
        UUID groupUuid = UUID.randomUUID();
        ModificationReferenceInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.COMPOSITE)
                .referencedId(sharedCompositeUuid)
                .referencedInfos(CompositeModificationInfos.builder().uuid(sharedCompositeUuid).build())
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(groupUuid, List.of(referenceInfos));
        UUID referenceUuid = saved.getFirst().getUuid();

        // 3. Verify the reference exists and is in the group
        ModificationInfos fetched = networkModificationRepository.getModificationInfo(referenceUuid);
        assertInstanceOf(ModificationReferenceInfos.class, fetched);
        assertEquals(sharedCompositeUuid, ((ModificationReferenceInfos) fetched).getReferencedId());

        // 4. Unshare the reference modification
        networkModificationService.unshareModification(referenceUuid, TEST_USER_ID);

        // 5. Verify the reference is deleted
        assertTrue(modificationRepository.findById(referenceUuid).isEmpty());

        // 6. Verify a new composite has been created in place of the reference
        List<ModificationInfos> groupModifications = networkModificationRepository.getModifications(groupUuid, true);
        assertEquals(1, groupModifications.size());
        assertEquals(CompositeModificationInfos.class, groupModifications.getFirst().getClass());
        assertNotEquals(sharedCompositeUuid, groupModifications.getFirst().getUuid());
    }

    @Test
    void shouldUnshareReferenceModificationInsideComposite() {
        // 1. Create a shared composite modification
        UUID sharedCompositeUuid = saveComposite(UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID())));

        // 2. Create a container composite that holds a reference to the shared composite
        UUID groupUuid = UUID.randomUUID();
        CompositeModificationInfos containerComposite = compositeModification(UUID.randomUUID(),
                List.of(referenceTo(sharedCompositeUuid)));
        List<ModificationInfos> containerSaved = networkModificationRepository.saveModifications(groupUuid,
                List.of(ModificationEntity.fromDTO(containerComposite)));
        UUID containerCompositeUuid = containerSaved.getFirst().getUuid();

        // 3. Get the reference UUID inside the container
        List<ModificationInfos> containerContent = networkModificationRepository.getModifications(containerCompositeUuid, true);
        UUID referenceUuid = containerContent.getFirst().getUuid();

        // 4. Unshare the reference modification
        networkModificationService.unshareModification(referenceUuid, TEST_USER_ID);

        // 5. Verify the reference is deleted
        assertTrue(modificationRepository.findById(referenceUuid).isEmpty());

        // 6. Verify a new composite has been created inside the container instead of the reference
        List<ModificationInfos> updatedContainerContent = networkModificationRepository.getModifications(containerCompositeUuid, true);
        assertEquals(1, updatedContainerContent.size());
        assertEquals(CompositeModificationInfos.class, updatedContainerContent.getFirst().getClass());
        assertNotEquals(sharedCompositeUuid, updatedContainerContent.getFirst().getUuid());
    }

    @Test
    void shouldThrowExceptionWhenUnshareNonReferenceModification() {
        // 1. Create a regular modification (not a reference)
        UUID groupUuid = UUID.randomUUID();
        LoadModificationInfos regularMod = dummyModification(UUID.randomUUID());
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(groupUuid, List.of(regularMod));
        UUID regularModUuid = saved.getFirst().getUuid();

        // 2. Attempt to unshare a non-reference modification
        assertThrows(Exception.class, () -> networkModificationService.unshareModification(regularModUuid, TEST_USER_ID),
                "Should throw exception for non-reference modification");
    }

    @Test
    void shouldThrowExceptionWhenUnshareNonexistentModification() {
        UUID nonexistentUuid = UUID.randomUUID();

        assertThrows(Exception.class, () -> networkModificationService.unshareModification(nonexistentUuid, TEST_USER_ID),
                "Should throw exception for nonexistent modification");
    }

    @Test
    void shouldPreserveCopiedCompositePropertiesWhenUnsharing() {
        // 1. Create a composite modification to be shared
        UUID sharedCompositeUuid = saveComposite(UUID.randomUUID(),
                List.of(dummyModification(UUID.randomUUID())));
        CompositeModificationInfos sharedComposite = (CompositeModificationInfos)
                networkModificationRepository.getModificationInfo(sharedCompositeUuid);

        // 2. Create a reference to the shared composite at root level
        UUID groupUuid = UUID.randomUUID();
        ModificationReferenceInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.COMPOSITE)
                .referencedId(sharedCompositeUuid)
                .referencedInfos(sharedComposite)
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(groupUuid, List.of(referenceInfos));
        UUID referenceUuid = saved.getFirst().getUuid();

        // 3. Unshare the reference
        networkModificationService.unshareModification(referenceUuid, TEST_USER_ID);

        // 4. Verify the copied composite has the same content as the original
        List<ModificationInfos> groupModifications = networkModificationRepository.getModifications(groupUuid, false);
        assertEquals(1, groupModifications.size());
        CompositeModificationInfos copiedComposite = (CompositeModificationInfos) groupModifications.getFirst();

        assertEquals(sharedComposite.getModificationsInfos().size(), copiedComposite.getModificationsInfos().size());
    }

    @Test
    void shouldPreserveModificationsOrderWhenUnsharing() {
        // 1. Create a shared composite with multiple modifications
        List<ModificationInfos> compositeChildren = List.of(
                dummyModification(UUID.randomUUID(), "equipment id 1"),
                dummyModification(UUID.randomUUID(), "equipment id 2"),
                dummyModification(UUID.randomUUID(), "equipment id 3")
        );
        UUID sharedCompositeUuid = saveComposite(UUID.randomUUID(), compositeChildren);
        CompositeModificationInfos sharedComposite = (CompositeModificationInfos)
                networkModificationRepository.getModificationInfo(sharedCompositeUuid);

        // 2. Create a reference to the shared composite
        UUID groupUuid = UUID.randomUUID();
        ModificationReferenceInfos referenceInfos = ModificationReferenceInfos.builder()
                .referenceType(ModificationReferenceInfos.Type.COMPOSITE)
                .referencedId(sharedCompositeUuid)
                .referencedInfos(sharedComposite)
                .stashed(false)
                .activated(true)
                .build();
        List<ModificationInfos> saved = networkModificationRepository.saveModificationInfos(groupUuid, List.of(referenceInfos));
        UUID referenceUuid = saved.getFirst().getUuid();

        // 3. Unshare the reference
        networkModificationService.unshareModification(referenceUuid, TEST_USER_ID);

        // 4. Verify the order of modifications is preserved. The uuids are different so they are compared according to they unique equipment id
        List<ModificationInfos> copiedCompositeContent = networkModificationRepository.getModifications(
                networkModificationRepository.getModifications(groupUuid, true).getFirst().getUuid(), false);

        assertEquals(sharedComposite.getModificationsInfos().size(), copiedCompositeContent.size());
        for (int i = 0; i < compositeChildren.size(); i++) {
            assertEquals(
                    ((LoadModificationInfos) sharedComposite.getModificationsInfos().get(i)).getEquipmentId(),
                    ((LoadModificationInfos) copiedCompositeContent.get(i)).getEquipmentId());
        }
    }
}
