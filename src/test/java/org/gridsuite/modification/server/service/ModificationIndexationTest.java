/**
 * Copyright (c) 2023, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import com.powsybl.network.store.iidm.impl.NetworkImpl;
import org.apache.commons.collections4.IterableUtils;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.EquipmentInfosService;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.*;

import static com.powsybl.iidm.network.VariantManagerConstants.INITIAL_VARIANT_ID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Tag("UnitTest")
class ModificationIndexationTest {

    // Need to mock the send reports
    @MockitoBean
    private ReportService reportService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @Mock
    private NetworkInfos networkInfos;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    @Mock
    private ReportInfos reportInfos;

    @Autowired
    private ModificationApplicationInfosRepository modificationApplicationInfosRepository;

    @Autowired
    private ModificationApplicationRepository modificationApplicationRepository;
    @Autowired
    private NetworkModificationService networkModificationService;

    @Autowired
    private SupervisionService supervisionService;

    UUID networkUuid = UUID.randomUUID();
    String variant2 = "variant_2";

    @BeforeEach
    void setUp() {
        cleanDB();
        Network network = NetworkCreation.createLoadNetwork(networkUuid, new NetworkFactoryImpl());
        network.getVariantManager().cloneVariant(INITIAL_VARIANT_ID, variant2);
        when(networkInfos.getNetwork()).thenReturn(network);
        when(networkInfos.getNetworkUuuid()).thenReturn(networkUuid);
        when(networkStoreService.getNetwork(eq(networkInfos.getNetworkUuuid()), any(PreloadingStrategy.class))).thenReturn(network);
    }

    @AfterEach
    void tearDown() {
        cleanDB();
    }

    private void cleanDB() {
        modificationRepository.deleteAll();
        modificationApplicationRepository.deleteAll();
        modificationApplicationInfosRepository.deleteAll();
    }

    @Test
    void testApplyCreatingModifications() {
        String newEquipmentId = "newLoad";
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos(newEquipmentId);
        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertTrue(modificationApplicationInfos.getFirst().getCreatedEquipmentIds().contains(newEquipmentId));
    }

    @Test
    void testApplyModifyingModifications() {
        String modifiedEquipmentId = "load1";
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
            .equipmentId(modifiedEquipmentId)
            .loadType(AttributeModification.toAttributeModification(LoadType.AUXILIARY, OperationType.SET))
            .build();
        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadModificationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertEquals(modifiedEquipmentId, modificationApplicationInfos.getFirst().getModifiedEquipmentIds().iterator().next());
    }

    @Test
    void testApplyDeletingModifications() {
        String deletedEquipmentId = "load1";
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
            .equipmentId(deletedEquipmentId)
            .equipmentType(IdentifiableType.LOAD)
            .build();
        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(equipmentDeletionInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertTrue(modificationApplicationInfos.getFirst().getDeletedEquipmentIds().contains(deletedEquipmentId));
    }

    @Test
    void testDuplicateModifications() {
        /*
        Create first modification then apply it on group 1
         */
        String newEquipmentId = "newLoad";
        // Need an entity with a lazyloaded subentity/collection to test that all required
        // data has been loaded during the completed transaction before applying modifications:
        // LoadCreationInfos does the job because it has the free properties Collection.
        // Is there a good way to add in this test that we have 2 short transactions
        // instead of one long idle transaction ?
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos(newEquipmentId);
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        // Need to remove the listener created in the last modifications application
        ((NetworkImpl) networkInfos.getNetwork()).getListeners().clear();

        /*
        Duplicate this modification to group 2, variant 2
         */
        UUID groupUuid2 = UUID.randomUUID();
        NetworkModificationsResult modificationsResult = networkModificationService.duplicateModifications(
            groupUuid2,
            null,
            modifications.stream().map(ModificationInfos::getUuid).toList(),
            List.of(new ModificationApplicationContext(networkInfos.getNetworkUuuid(), variant2, UUID.randomUUID(), UUID.randomUUID()))
        ).join();

        /*
        check results in database and in elasticsearch
         */
        List<UUID> expectedModificationUuids = List.of(modifications.getFirst().getUuid(), modificationsResult.modificationUuids().getFirst());
        List<UUID> expectedGroupUuids = List.of(groupUuid1, groupUuid2);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertThat(modificationApplicationEntities.stream().map(m -> m.getModification().getId()).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);
        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getModificationUuid).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);

        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getGroupUuid).toList()).usingRecursiveComparison().isEqualTo(expectedGroupUuids);
        modificationApplicationInfos.forEach(applicationInfo ->
                assertTrue(applicationInfo.getCreatedEquipmentIds().contains(newEquipmentId))
        );
    }

    @Test
    void testMoveModifications() {
        /*
        Create first modification then apply it on group 1
         */
        String newEquipmentId = "newLoad";
        // Need an entity with a lazyloaded subentity/collection to test that all required
        // data has been loaded during the completed transaction before applying modifications:
        // LoadCreationInfos does the job because it has the free properties Collection.
        // Is there a good way to add in this test that we have 2 short transactions
        // instead of one long idle transaction ?
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos(newEquipmentId);
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        // Need to remove the listener created in the last modifications application
        ((NetworkImpl) networkInfos.getNetwork()).getListeners().clear();

        /*
        Move this modification to group 2, variant 2
         */
        UUID groupUuid2 = UUID.randomUUID();
        NetworkModificationsResult modificationsResult = networkModificationService.moveModifications(
            groupUuid2,
            groupUuid1,
            null,
            modifications.stream().map(ModificationInfos::getUuid).toList(),
            List.of(new ModificationApplicationContext(networkInfos.getNetworkUuuid(), variant2, UUID.randomUUID(), UUID.randomUUID())),
            true
        ).join();

        /*
        check results in database and in elasticsearch
         */
        List<UUID> expectedModificationUuids = List.of(modificationsResult.modificationUuids().getFirst());
        List<UUID> expectedGroupUuids = List.of(groupUuid2);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertThat(modificationApplicationEntities.stream().map(m -> m.getModification().getId()).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);
        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getModificationUuid).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);

        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getGroupUuid).toList()).usingRecursiveComparison().isEqualTo(expectedGroupUuids);
        modificationApplicationInfos.forEach(applicationInfo -> assertTrue(applicationInfo.getCreatedEquipmentIds().contains(newEquipmentId)));
    }

    @Test
    void testInsertCompositeModifications() {
        /*
        Create first modification then apply it on group 1
         */
        String newEquipmentId = "newLoad";
        // Need an entity with a lazyloaded subentity/collection to test that all required
        // data has been loaded during the completed transaction before applying modifications:
        // LoadCreationInfos does the job because it has the free properties Collection.
        // Is there a good way to add in this test that we have 2 short transactions
        // instead of one long idle transaction ?
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos(newEquipmentId);
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        // Create the composite modification to pass later to ?action=insert
        UUID compositeUuid = networkModificationService.createNetworkCompositeModification(
                modifications.stream().map(ModificationInfos::getUuid).toList()
        );

        // Need to remove the listener created in the last modifications application
        ((NetworkImpl) networkInfos.getNetwork()).getListeners().clear();

        /*
        Split this composite and insert the contained modifications to group 2, variant 2
         */
        UUID groupUuid2 = UUID.randomUUID();
        NetworkModificationsResult modificationsResult = networkModificationService.splitCompositeModifications(
            groupUuid2,
            List.of(compositeUuid),
            List.of(new ModificationApplicationContext(networkInfos.getNetworkUuuid(), variant2, UUID.randomUUID(), UUID.randomUUID()))
        ).join();

        /*
        check results in database and in elasticsearch
         */
        List<UUID> expectedModificationUuids = List.of(modifications.getFirst().getUuid(), modificationsResult.modificationUuids().getFirst());
        List<UUID> expectedGroupUuids = List.of(groupUuid1, groupUuid2);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertThat(modificationApplicationEntities.stream().map(m -> m.getModification().getId()).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);
        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getModificationUuid).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);

        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getGroupUuid).toList()).usingRecursiveComparison().isEqualTo(expectedGroupUuids);
        modificationApplicationInfos.forEach(applicationInfo -> assertTrue(applicationInfo.getCreatedEquipmentIds().contains(newEquipmentId)));
    }

    @Test
    void testDeleteModifications() {
        /*
        Create modification then apply it on group 1
         */
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos("newLoad");
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        /*
        Delete this modification
         */
        networkModificationService.deleteNetworkModifications(
            groupUuid1,
            modifications.stream().map(ModificationInfos::getUuid).toList()
        );

        /*
        check results in database and in elasticsearch
         */

        assertEquals(Collections.emptyList(), modificationApplicationRepository.findAll());
        assertEquals(Collections.emptyList(), IterableUtils.toList(modificationApplicationInfosRepository.findAll()));
    }

    @Test
    void testDeleteModificationGroup() {
        /*
        Create modification then apply it on group 1
         */
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos("newLoad");
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        /*
        Delete modification group
         */
        networkModificationService.deleteModificationGroup(
            groupUuid1,
            true
        );

        /*
        check results in database and in elasticsearch
         */

        assertEquals(Collections.emptyList(), modificationApplicationRepository.findAll());
        assertEquals(Collections.emptyList(), IterableUtils.toList(modificationApplicationInfosRepository.findAll()));
    }

    @Test
    void testReindexAll() {
        List<LoadCreationInfos> loadCreationInfosList = List.of(
            createLoadCreationInfos("newLoad"),
            createLoadCreationInfos("newLoad2"),
            createLoadCreationInfos("newLoad3")
        );
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid1, loadCreationInfosList.stream().map(ModificationEntity::fromDTO).toList());

        // apply modifications to index them
        TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid1, modifications, reportInfos), networkInfos);

        // assert they are both stored in ES and in postgres
        assertEquals(3, modificationApplicationRepository.findAll().size());
        List<ModificationApplicationInfos> applicationBeforeReindexing = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertEquals(3, applicationBeforeReindexing.size());

        // remove elasticsearch content
        modificationApplicationInfosRepository.deleteAll();
        assertEquals(0, IterableUtils.toList(modificationApplicationInfosRepository.findAll()).size());

        // reindex all modification to check they are all reindexed with the same values
        supervisionService.reindexByNetworkUuid(networkInfos.getNetworkUuuid());
        assertThat(applicationBeforeReindexing).usingRecursiveComparison().isEqualTo(IterableUtils.toList(modificationApplicationInfosRepository.findAll()));
        assertEquals(3, IterableUtils.toList(modificationApplicationInfosRepository.findAll()).size());

    }

    @Test
    void testMultiplePropertiesModificationOnSingleEquipment() {
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
            .equipmentId("load1")
            .p0(AttributeModification.toAttributeModification(43D, OperationType.SET))
            .q0(AttributeModification.toAttributeModification(2D, OperationType.SET))
            .build();

        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadModificationInfos)));
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        ModificationApplicationEntity modificationApplicationEntity = modificationApplicationRepository.findAll().getFirst();
        ModificationApplicationInfos modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll()).getFirst();

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntity.getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getGroupUuid());

        assertEquals(1, modificationApplicationEntity.getModifiedEquipmentIds().size());
        assertEquals(1, modificationApplicationInfos.getModifiedEquipmentIds().size());
    }

    @Test
    void testSwitchModification() {
        EquipmentAttributeModificationInfos openSwitchModification = EquipmentAttributeModificationInfos.builder()
            .equipmentId("v1d1")
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue("open")
            .build();

        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(openSwitchModification)));
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        assertEquals(1, modificationRepository.getModifications(groupUuid, true, true).size());
        assertEquals(Collections.emptyList(), modificationApplicationRepository.findAll());
        assertEquals(Collections.emptyList(), IterableUtils.toList(modificationApplicationInfosRepository.findAll()));
    }

    @Test
    void testUpdateSubstationName() {
        SubstationModificationInfos substationModificationInfos = SubstationModificationInfos.builder()
            .equipmentId("s1")
            .equipmentName(AttributeModification.toAttributeModification("newSubstationName", OperationType.SET))
            .build();

        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(substationModificationInfos)));
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        ModificationApplicationEntity modificationApplicationEntity = modificationApplicationRepository.findAll().getFirst();
        ModificationApplicationInfos modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll()).getFirst();

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntity.getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getGroupUuid());

        assertEquals(1, modificationApplicationEntity.getModifiedEquipmentIds().size());
        assertEquals(1, modificationApplicationInfos.getModifiedEquipmentIds().size());

        assertEquals("s1", modificationApplicationEntity.getModifiedEquipmentIds().stream().findAny().get());
        assertEquals("s1", modificationApplicationInfos.getModifiedEquipmentIds().stream().findAny().get());
    }

    private LoadCreationInfos createLoadCreationInfos(String loadId) {
        return LoadCreationInfos.builder()
            .stashed(false)
            .loadType(LoadType.FICTITIOUS)
            .p0(300.0)
            .q0(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .voltageLevelId("v1")
            .equipmentId(loadId)
            .busOrBusbarSectionId("1.1")
            .build();
    }

    @Test
    void testUpdateModificationProperties() {
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
                .equipmentId("load1")
                .properties(List.of(FreePropertyInfos.builder().name("loadPropertyName").value("loadPropertyValue").build()))
                .build();

        UUID groupUuid = UUID.randomUUID();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadModificationInfos)));
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
        assertNotNull(result);

        ModificationApplicationEntity modificationApplicationEntity = modificationApplicationRepository.findAll().getFirst();
        ModificationApplicationInfos modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll()).getFirst();

        assertEquals(modifications.getFirst().getUuid(), modificationApplicationEntity.getModification().getId());
        assertEquals(modifications.getFirst().getUuid(), modificationApplicationInfos.getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getGroupUuid());

        assertEquals(1, modificationApplicationEntity.getModifiedEquipmentIds().size());
        assertEquals(1, modificationApplicationInfos.getModifiedEquipmentIds().size());

        Optional<String> anyModifiedEquipmentIdEntity = modificationApplicationEntity.getModifiedEquipmentIds().stream().findAny();
        assertTrue(anyModifiedEquipmentIdEntity.isPresent());
        assertEquals("load1", anyModifiedEquipmentIdEntity.get());

        Optional<String> anyModifiedEquipmentIdInfos = modificationApplicationInfos.getModifiedEquipmentIds().stream().findAny();
        assertTrue(anyModifiedEquipmentIdInfos.isPresent());
        assertEquals("load1", anyModifiedEquipmentIdEntity.get());
    }

    @Test
    void testUpdateModificationPropertiesWithNotIndexedEquipment() {
        try (MockedStatic<EquipmentInfosService> mocked = Mockito.mockStatic(EquipmentInfosService.class)) {
            mocked.when(EquipmentInfosService::getIndexedEquipmentTypes)
                    .thenReturn(Set.of());

            SubstationModificationInfos substationModificationInfos = SubstationModificationInfos.builder()
                    .equipmentId("s1")
                    .properties(List.of(FreePropertyInfos.builder().name("s1PropertyName").value("s1PropertyValue").build()))
                    .build();

            UUID groupUuid = UUID.randomUUID();
            List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(substationModificationInfos)));
            NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator, new ModificationApplicationGroup(groupUuid, modifications, reportInfos), networkInfos);
            assertNotNull(result);

            assertEquals(1, modificationRepository.getModifications(groupUuid, true, true).size());
            assertEquals(Collections.emptyList(), modificationApplicationRepository.findAll());
            assertEquals(Collections.emptyList(), IterableUtils.toList(modificationApplicationInfosRepository.findAll()));
        }
    }
}
