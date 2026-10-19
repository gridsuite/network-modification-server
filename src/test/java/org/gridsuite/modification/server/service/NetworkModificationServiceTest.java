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
import org.gridsuite.modification.server.dto.ModificationApplicationContext;
import org.gridsuite.modification.server.dto.ModificationReferenceData;
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
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
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

    @MockitoBean
    private DirectoryService directoryService;

    @MockitoBean
    private NotificationService notificationService;

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

        networkModificationService.stashNetworkModifications(groupUuid, List.of(childUuid), "userId");

        // the modification was moved out of the composite, into the group, before being stashed
        assertNull(modificationRepository.findCompositeContainerIdByModificationId(childUuid));
        assertEquals(1, networkModificationRepository.getModificationsCount(groupUuid, true));
        assertTrue(networkModificationRepository.getModifications(groupUuid, true, StashedFilter.STASHED).stream()
                .anyMatch(modificationInfos -> modificationInfos.getUuid().equals(childUuid)));
        // the composite itself is left in place, now empty
        assertEquals(List.of(compositeUuid), modificationRepository.findAllChildrenUuids(compositeUuid));
    }

    @Test
    void shouldDeleteReferencesInsteadOfStashingThem() {
        UUID sharedUuid = saveComposite(UUID.randomUUID(), List.of(dummyModification(UUID.randomUUID())));
        UUID groupUuid = UUID.randomUUID();
        // reference ; plain ; composite ── (reference, leaf) ; other composite ── reference
        List<ModificationInfos> saved = networkModificationRepository.saveModifications(groupUuid, List.of(
                ModificationEntity.fromDTO(referenceTo(sharedUuid)),
                ModificationEntity.fromDTO(dummyModification(UUID.randomUUID())),
                ModificationEntity.fromDTO(compositeModification(UUID.randomUUID(), List.of(referenceTo(sharedUuid), dummyModification(UUID.randomUUID())))),
                ModificationEntity.fromDTO(compositeModification(UUID.randomUUID(), List.of(referenceTo(sharedUuid))))));
        UUID rootReferenceUuid = saved.get(0).getUuid();
        UUID plainUuid = saved.get(1).getUuid();
        CompositeModificationInfos stashedComposite = (CompositeModificationInfos) saved.get(2);
        UUID referenceInStashedCompositeUuid = stashedComposite.getModificationsInfos().get(0).getUuid();
        UUID leafUuid = stashedComposite.getModificationsInfos().get(1).getUuid();
        CompositeModificationInfos keptComposite = (CompositeModificationInfos) saved.get(3);
        UUID selectedNestedReferenceUuid = keptComposite.getModificationsInfos().getFirst().getUuid();

        networkModificationService.stashNetworkModifications(groupUuid,
                List.of(rootReferenceUuid, plainUuid, stashedComposite.getUuid(), selectedNestedReferenceUuid), "userId");

        // the references, selected or nested in a stashed composite, are deleted and unlinked from the shared modification
        List<UUID> referenceUuids = List.of(rootReferenceUuid, referenceInStashedCompositeUuid, selectedNestedReferenceUuid);
        referenceUuids.forEach(uuid -> assertFalse(modificationRepository.existsById(uuid)));
        verify(directoryService).removeElementReferences(argThat(references -> references.size() == 3
                && references.stream().map(ModificationReferenceData::modificationUuid).toList().containsAll(referenceUuids)), eq("userId"));
        // the others are stashed, the composite without its reference
        assertEquals(Set.of(plainUuid, stashedComposite.getUuid()),
                networkModificationRepository.getModifications(groupUuid, true, StashedFilter.STASHED).stream()
                        .map(ModificationInfos::getUuid).collect(Collectors.toSet()));
        assertEquals(List.of(leafUuid), modificationRepository.findAllDescendantModificationIdsByContainerIds(List.of(stashedComposite.getUuid())));
        // the composite holding the selected reference stays in place, now empty
        assertTrue(modificationRepository.findAllDescendantModificationIdsByContainerIds(List.of(keptComposite.getUuid())).isEmpty());
        assertEquals(List.of(keptComposite.getUuid()),
                networkModificationRepository.getModifications(groupUuid, true, StashedFilter.UNSTASHED).stream().map(ModificationInfos::getUuid).toList());
    }

    @Test
    void shouldDeleteReferenceStashedFromASharedComposite() {
        UUID otherSharedUuid = saveComposite(UUID.randomUUID(), List.of(dummyModification(UUID.randomUUID())));
        // shared ── reference to the other shared
        CompositeModificationInfos shared = (CompositeModificationInfos) networkModificationRepository.saveModifications(UUID.randomUUID(),
                List.of(ModificationEntity.fromDTO(compositeModification(UUID.randomUUID(), List.of(referenceTo(otherSharedUuid)))))).getFirst();
        UUID referenceInSharedUuid = shared.getModificationsInfos().getFirst().getUuid();
        UUID groupUuid = UUID.randomUUID();
        networkModificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(referenceTo(shared.getUuid()))));

        // the reference is reached from the group through the shared composite
        networkModificationService.stashNetworkModifications(groupUuid, List.of(referenceInSharedUuid), "userId");

        assertFalse(modificationRepository.existsById(referenceInSharedUuid));
        assertTrue(modificationRepository.findAllDescendantModificationIdsByContainerIds(List.of(shared.getUuid())).isEmpty());
    }

    @Test
    void shouldDeleteAllReferencesOfAGroup() {
        UUID sharedUuid = saveComposite(UUID.randomUUID(), List.of(dummyModification(UUID.randomUUID())));
        UUID groupUuid = UUID.randomUUID();
        // reference ; plain ; composite ── (reference, leaf)
        List<ModificationInfos> saved = networkModificationRepository.saveModifications(groupUuid, List.of(
                ModificationEntity.fromDTO(referenceTo(sharedUuid)),
                ModificationEntity.fromDTO(dummyModification(UUID.randomUUID())),
                ModificationEntity.fromDTO(compositeModification(UUID.randomUUID(), List.of(referenceTo(sharedUuid), dummyModification(UUID.randomUUID()))))));
        UUID rootReferenceUuid = saved.get(0).getUuid();
        UUID plainUuid = saved.get(1).getUuid();
        CompositeModificationInfos composite = (CompositeModificationInfos) saved.get(2);
        UUID nestedReferenceUuid = composite.getModificationsInfos().get(0).getUuid();
        UUID leafUuid = composite.getModificationsInfos().get(1).getUuid();

        networkModificationService.deleteModificationReferences(groupUuid, "userId");

        List<UUID> referenceUuids = List.of(rootReferenceUuid, nestedReferenceUuid);
        referenceUuids.forEach(uuid -> assertFalse(modificationRepository.existsById(uuid)));
        verify(directoryService).removeElementReferences(argThat(references -> references.size() == 2
                && references.stream().map(ModificationReferenceData::modificationUuid).toList().containsAll(referenceUuids)), eq("userId"));
        assertEquals(List.of(plainUuid, composite.getUuid()),
                networkModificationRepository.getModifications(groupUuid, true, StashedFilter.UNSTASHED).stream().map(ModificationInfos::getUuid).toList());
        assertEquals(List.of(leafUuid), modificationRepository.findAllDescendantModificationIdsByContainerIds(List.of(composite.getUuid())));
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
}
