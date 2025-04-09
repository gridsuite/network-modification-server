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
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.apache.commons.collections4.IterableUtils;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.ModificationApplicationGroup;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReportInfos;
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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.when;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Tag("UnitTest")
class ModificationIndexationTest {

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @Autowired
    private NetworkModificationApplicator networkModificationApplicator;

    @Mock
    private NetworkInfos networkInfos;

    @Mock
    private ReportInfos reportInfos;

    @Autowired
    private ModificationApplicationInfosRepository modificationApplicationInfosRepository;

    @Autowired
    private ModificationApplicationRepository modificationApplicationRepository;

    @BeforeEach
    void setUp() {
        cleanDB();
        Network network = NetworkCreation.createLoadNetwork(UUID.randomUUID(), new NetworkFactoryImpl());
        when(networkInfos.getNetwork()).thenReturn(network);
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
}
