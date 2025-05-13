/**
 * Copyright (c) 2025, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.gridsuite.modification.server.service;

import com.powsybl.iidm.network.Country;
import com.powsybl.iidm.network.LoadType;
import com.powsybl.iidm.network.Network;
import com.powsybl.iidm.network.extensions.ConnectablePosition;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import com.powsybl.network.store.iidm.impl.NetworkFactoryImpl;
import org.apache.commons.collections4.IterableUtils;
import org.gridsuite.modification.dto.*;
import org.gridsuite.modification.server.dto.*;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
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

import java.util.*;
import java.util.stream.Collectors;

import static com.powsybl.iidm.network.VariantManagerConstants.INITIAL_VARIANT_ID;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE)
@Tag("UnitTest")
class ModificationSearchTest {

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

    @Autowired
    private SupervisionService supervisionService;

    UUID networkUuid = UUID.randomUUID();
    UUID groupUuid = UUID.randomUUID();
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
    void testSearchModificationsByGroup() {
        // Substation creation for equipment Id substationId
        SubstationCreationInfos substationCreationInfos = SubstationCreationInfos.builder()
                .equipmentId("substationId")
                .equipmentName("substationName")
                .country(Country.TD)
                .properties(null)
                .build();

        // Modification substation for equipment Id substationId
        SubstationModificationInfos substationModificationInfos = SubstationModificationInfos.builder()
                .equipmentId("substationId")
                .equipmentName(AttributeModification.toAttributeModification("newSubstationName", OperationType.SET))
                .build();

        // Load creation for equipment Id newLoadId
        LoadCreationInfos loadCreationInfos = createLoadCreationInfos("newLoadId");
        List<ModificationEntity> entities = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(substationCreationInfos), ModificationEntity.fromDTO(substationModificationInfos), ModificationEntity.fromDTO(loadCreationInfos)));

        // apply modifications to index them on groupUuid
        NetworkModificationResult result = networkModificationApplicator.applyModifications(new ModificationApplicationGroup(groupUuid, entities, reportInfos), networkInfos);
        assertNotNull(result);
        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertEquals(3, modificationApplicationInfos.size());

        // Search with network UUID and userInput 'Id' to search all equipmentIds containing 'Id'
        Set<ModificationApplicationInfos> hits = new HashSet<>();
        hits.addAll(networkModificationService.searchNetworkModificationsResult(networkInfos.getNetworkUuuid(), "Id"));
        assertEquals(3, hits.size());

        // Verify that all UUIDs from the hits are present in modificationApplicationInfos
        Set<UUID> expectedUuids = modificationApplicationInfos.stream()
                .map(ModificationApplicationInfos::getModificationUuid)
                .collect(Collectors.toSet());
        for (ModificationApplicationInfos hit : hits) {
            assertTrue(expectedUuids.contains(hit.getModificationUuid()));
        }
        hits.clear();

        // search with userInput 'load'
        hits.addAll(networkModificationService.searchNetworkModificationsResult(networkInfos.getNetworkUuuid(), "load"));
        assertEquals(1, hits.size());
        ModificationApplicationInfos hit = hits.iterator().next();

        List<ModificationApplicationInfos> loadModificationsApplicationInfos = modificationApplicationInfos.stream().filter(modificationApplicationInfos1 -> modificationApplicationInfos1.getCreatedEquipmentIds().contains(loadCreationInfos.getEquipmentId())).toList();
        assertEquals(1, loadModificationsApplicationInfos.size());
        ModificationApplicationInfos loadModificationApplicationInfos = loadModificationsApplicationInfos.getFirst();
        assertEquals(hit.getModificationUuid(), loadModificationApplicationInfos.getModificationUuid());
        hits.clear();

        // search using an equipment identifier that doesn't exist ("notFound")
        hits.addAll(networkModificationService.searchNetworkModificationsResult(networkInfos.getNetworkUuuid(), "notFound"));
        assertEquals(0, hits.size());
        hits.clear();

        // search using an equipment identifier on non existing network
        hits.addAll(networkModificationService.searchNetworkModificationsResult(UUID.randomUUID(), "Id"));
        assertEquals(0, hits.size());
        hits.clear();

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
}
