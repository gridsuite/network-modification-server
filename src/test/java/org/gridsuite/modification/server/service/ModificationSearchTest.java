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
import org.gridsuite.modification.server.utils.assertions.Assertions;
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
import static org.assertj.core.api.Assertions.assertThat;

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
    void testSearchModificationsWithAccentsAndSpecialChars() {
        String equipmentIdAccent = "TéstÉquipé";
        String equipmentIdSpecial = "SpecialChars@#d'Essai";

        SubstationCreationInfos creationWithAccent = SubstationCreationInfos.builder()
                .equipmentId(equipmentIdAccent)
                .equipmentName("accent")
                .country(Country.FR)
                .properties(null)
                .build();

        SubstationCreationInfos creationWithSpecial = SubstationCreationInfos.builder()
                .equipmentId(equipmentIdSpecial)
                .equipmentName("SpecialChars")
                .country(Country.FR)
                .properties(null)
                .build();

        List<ModificationEntity> entities = modificationRepository.saveModifications(
                groupUuid,
                List.of(
                        ModificationEntity.fromDTO(creationWithAccent),
                        ModificationEntity.fromDTO(creationWithSpecial)
                )
        );

        NetworkModificationResult result = networkModificationApplicator.applyModifications(
                new ModificationApplicationGroup(groupUuid, entities, reportInfos),
                networkInfos
        );
        assertThat(result).isNotNull();

        List<ModificationApplicationInfos> allModifications = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertThat(allModifications).hasSize(2);

        // search by userInput containing accent
        List<ModificationApplicationInfos> hitsAccent2 = networkModificationService.searchNetworkModificationInfos(networkInfos.getNetworkUuuid(), "test");
        assertThat(hitsAccent2).hasSize(1);
        assertThat(hitsAccent2.getFirst().getCreatedEquipmentIds()).contains(equipmentIdAccent);

        // search by term containing special chars
        List<ModificationApplicationInfos> hitsSpecial = networkModificationService.searchNetworkModificationInfos(networkInfos.getNetworkUuuid(), "SpecialChars@");
        assertThat(hitsSpecial).hasSize(1);
        assertThat(hitsSpecial.getFirst().getCreatedEquipmentIds()).contains(equipmentIdSpecial);
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

        NetworkModificationResult result = networkModificationApplicator.applyModifications(
                new ModificationApplicationGroup(groupUuid, entities, reportInfos),
                networkInfos
        );
        assertThat(result).isNotNull();

        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertThat(modificationApplicationInfos).hasSize(3);

        // Search with network UUID and userInput 'Id' to search all equipmentIds containing 'Id'
        List<ModificationApplicationInfos> hits = networkModificationService
                .searchNetworkModificationInfos(networkInfos.getNetworkUuuid(), "Id");

        assertThat(hits)
                .hasSize(3)
                .extracting(ModificationApplicationInfos::getModificationUuid)
                .allMatch(modificationApplicationInfos.stream()
                        .map(ModificationApplicationInfos::getModificationUuid)
                        .collect(Collectors.toSet())::contains);

        // search with userInput 'load'
        hits = networkModificationService.searchNetworkModificationInfos(networkInfos.getNetworkUuuid(), "load");
        assertThat(hits).hasSize(1);

        ModificationApplicationInfos hit = hits.getFirst();

        List<ModificationApplicationInfos> loadModificationsApplicationInfos = modificationApplicationInfos.stream()
                .filter(infos -> infos.getCreatedEquipmentIds().contains(loadCreationInfos.getEquipmentId()))
                .toList();
        assertThat(loadModificationsApplicationInfos).hasSize(1);

        ModificationApplicationInfos loadModificationApplicationInfos = loadModificationsApplicationInfos.getFirst();
        assertThat(hit.getModificationUuid()).isEqualTo(loadModificationApplicationInfos.getModificationUuid());

        // search using an equipment identifier that doesn't exist ("notFound")
        hits = networkModificationService.searchNetworkModificationInfos(networkInfos.getNetworkUuuid(), "notFound");
        Assertions.assertThat(hits).isEmpty();

        // search using an equipment identifier on non-existing network
        hits = networkModificationService.searchNetworkModificationInfos(UUID.randomUUID(), "Id");
        assertThat(hits).isEmpty();
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
