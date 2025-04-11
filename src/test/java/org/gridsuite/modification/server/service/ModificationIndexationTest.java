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
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.entities.ModificationApplicationEntity;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import java.util.List;
import java.util.UUID;

import static com.powsybl.iidm.network.VariantManagerConstants.INITIAL_VARIANT_ID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Tag("UnitTest")
class ModificationIndexationTest {

    // Need to mock the send reports
    @MockBean
    private ReportService reportService;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @Mock
    private NetworkInfos networkInfos;

    @MockBean
    private NetworkStoreService networkStoreService;

    @Mock
    private ReportInfos reportInfos;

    @Autowired
    private ModificationApplicationInfosRepository modificationApplicationInfosRepository;

    @Autowired
    private ModificationApplicationRepository modificationApplicationRepository;
    @Autowired
    private NetworkModificationService networkModificationService;

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
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .stashed(false)
            .loadType(LoadType.FICTITIOUS)
            .p0(300.0)
            .q0(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .voltageLevelId("v1")
            .equipmentId(newEquipmentId)
            .busOrBusbarSectionId("1.1")
            .build();
        UUID groupUuid = UUID.randomUUID();
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid, entities, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(entities.getFirst().getId(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(entities.getFirst().getId(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertEquals(newEquipmentId, modificationApplicationInfos.getFirst().getCreatedEquipmentIds().getFirst());
    }

    @Test
    void testApplyModifyingModifications() {
        String modifiedEquipmentId = "load1";
        LoadModificationInfos loadModificationInfos = LoadModificationInfos.builder()
            .equipmentId(modifiedEquipmentId)
            .loadType(AttributeModification.toAttributeModification(LoadType.AUXILIARY, OperationType.SET))
            .build();
        UUID groupUuid = UUID.randomUUID();
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(loadModificationInfos)));

        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid, entities, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(entities.getFirst().getId(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(entities.getFirst().getId(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertEquals(modifiedEquipmentId, modificationApplicationInfos.getFirst().getModifiedEquipmentIds().getFirst());
    }

    @Test
    void testApplyDeletingModifications() {
        String deletedEquipmentId = "load1";
        EquipmentDeletionInfos equipmentDeletionInfos = EquipmentDeletionInfos.builder()
            .equipmentId(deletedEquipmentId)
            .equipmentType(IdentifiableType.LOAD)
            .build();
        UUID groupUuid = UUID.randomUUID();
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(equipmentDeletionInfos)));

        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid, entities, reportInfos), networkInfos);
        assertNotNull(result);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertEquals(1, modificationApplicationEntities.size());
        assertEquals(1, modificationApplicationInfos.size());

        assertEquals(entities.getFirst().getId(), modificationApplicationEntities.getFirst().getModification().getId());
        assertEquals(entities.getFirst().getId(), modificationApplicationInfos.getFirst().getModificationUuid());
        assertEquals(groupUuid, modificationApplicationInfos.getFirst().getGroupUuid());
        assertEquals(deletedEquipmentId, modificationApplicationInfos.getFirst().getDeletedEquipmentIds().getFirst());
    }

    @Test
    void testDuplicateModifications() {
        /*
        Create first modification then apply it on group 1
         */
        String newEquipmentId = "newLoad";
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .stashed(false)
            .loadType(LoadType.FICTITIOUS)
            .p0(300.0)
            .q0(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .voltageLevelId("v1")
            .equipmentId(newEquipmentId)
            .busOrBusbarSectionId("1.1")
            .build();
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid1, entities, reportInfos), networkInfos);
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
            entities.stream().map(ModificationEntity::getId).toList(),
            List.of(new ModificationApplicationContext(networkInfos.getNetworkUuuid(), variant2, UUID.randomUUID(), UUID.randomUUID()))
        );

        /*
        check results in database and in elasticsearch
         */
        List<UUID> expectedModificationUuids = List.of(entities.getFirst().getId(), modificationsResult.modificationUuids().getFirst());
        List<UUID> expectedGroupUuids = List.of(groupUuid1, groupUuid2);

        List<ModificationApplicationEntity> modificationApplicationEntities = modificationApplicationRepository.findAll();
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());

        assertThat(modificationApplicationEntities.stream().map(m -> m.getModification().getId()).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);
        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getModificationUuid).toList()).usingRecursiveComparison().isEqualTo(expectedModificationUuids);

        assertThat(modificationApplicationInfos.stream().map(ModificationApplicationInfos::getGroupUuid).toList()).usingRecursiveComparison().isEqualTo(expectedGroupUuids);
        modificationApplicationInfos.forEach(applicationInfo -> assertEquals(newEquipmentId, applicationInfo.getCreatedEquipmentIds().getFirst()));
    }

    @Test
    void testMoveModifications() {
        /*
        Create first modification then apply it on group 1
         */
        String newEquipmentId = "newLoad";
        LoadCreationInfos loadCreationInfos = LoadCreationInfos.builder()
            .stashed(false)
            .loadType(LoadType.FICTITIOUS)
            .p0(300.0)
            .q0(50.0)
            .connectionName("bottom")
            .connectionDirection(ConnectablePosition.Direction.BOTTOM)
            .voltageLevelId("v1")
            .equipmentId(newEquipmentId)
            .busOrBusbarSectionId("1.1")
            .build();
        UUID groupUuid1 = UUID.randomUUID();
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid1, List.of(ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid1, entities, reportInfos), networkInfos);
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
            entities.stream().map(ModificationEntity::getId).toList(),
            List.of(new ModificationApplicationContext(networkInfos.getNetworkUuuid(), variant2, UUID.randomUUID(), UUID.randomUUID())),
            true
        );

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
        modificationApplicationInfos.forEach(applicationInfo -> assertEquals(newEquipmentId, applicationInfo.getCreatedEquipmentIds().getFirst()));
    }
}
