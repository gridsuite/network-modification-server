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
import org.gridsuite.modification.server.dto.ModificationsSearchResult;
import org.gridsuite.modification.server.dto.NetworkInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.ReportInfos;
import org.gridsuite.modification.server.dto.elasticsearch.ModificationApplicationInfos;
import org.gridsuite.modification.server.elasticsearch.ModificationApplicationInfosRepository;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.modifications.NetworkModificationApplicator;
import org.gridsuite.modification.server.repositories.ModificationApplicationRepository;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.assertions.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import static com.powsybl.iidm.network.VariantManagerConstants.INITIAL_VARIANT_ID;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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
    private NetworkModificationRepository networkModificationRepository;

    UUID networkUuid = UUID.randomUUID();
    UUID groupUuid = UUID.randomUUID();
    UUID group2Uuid = UUID.randomUUID();
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

        List<ModificationInfos> modifications = modificationRepository.saveModifications(
                groupUuid,
                List.of(
                        ModificationEntity.fromDTO(creationWithAccent),
                        ModificationEntity.fromDTO(creationWithSpecial)
                )
        );

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator,
                TestUtils.groupOnAnyRootNetwork(groupUuid, modifications, reportInfos),
                networkInfos
        );
        assertThat(result).isNotNull();

        List<ModificationApplicationInfos> allModifications = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertThat(allModifications).hasSize(2);

        // search by userInput containing accent
        Map<UUID, List<ModificationsSearchResult>> hitsAccent2 = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "test");
        assertThat(hitsAccent2).hasSize(1);
        assertNotNull(hitsAccent2.get(groupUuid));
        List<ModificationsSearchResult> searchResults = hitsAccent2.get(groupUuid);
        assertThat(searchResults).hasSize(1);
        assertThat(searchResults.getFirst().getImpactedEquipmentIds()).contains(equipmentIdAccent);

        // search by term containing special chars
        Map<UUID, List<ModificationsSearchResult>> hitsSpecial = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "SpecialChars@");
        assertThat(hitsSpecial).hasSize(1);
        assertNotNull(hitsSpecial.get(groupUuid));
        searchResults = hitsSpecial.get(groupUuid);
        assertThat(searchResults).hasSize(1);
        assertThat(searchResults.getFirst().getImpactedEquipmentIds()).contains(equipmentIdSpecial);
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

        // Save modifications
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(substationCreationInfos), ModificationEntity.fromDTO(
                substationModificationInfos), ModificationEntity.fromDTO(loadCreationInfos)));

        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator,
                TestUtils.groupOnAnyRootNetwork(groupUuid, modifications, reportInfos),
                networkInfos
        );
        assertThat(result).isNotNull();

        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertThat(modificationApplicationInfos).hasSize(3);

        // Search with network UUID and userInput 'Id' to search all equipmentIds containing 'Id'
        Map<UUID, List<ModificationsSearchResult>> hits = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "Id");
        assertThat(hits).hasSize(1);
        assertNotNull(hits.get(groupUuid));
        List<ModificationsSearchResult> searchResults = hits.get(groupUuid);
        assertThat(searchResults).hasSize(3);
        Set<UUID> modificationUuids = modificationApplicationInfos.stream().map(ModificationApplicationInfos::getModificationUuid).collect(Collectors.toSet());
        assertThat(searchResults).extracting(ModificationsSearchResult::getModificationUuid).allMatch(modificationUuids::contains);

        // search with userInput 'load'
        hits = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "load");
        assertThat(hits).hasSize(1);
        assertNotNull(hits.get(groupUuid));
        searchResults = hits.get(groupUuid);
        assertThat(searchResults).hasSize(1);

        List<ModificationApplicationInfos> loadModificationsApplicationInfos = modificationApplicationInfos.stream()
                .filter(infos -> infos.getCreatedEquipmentIds().contains(loadCreationInfos.getEquipmentId()))
                .toList();
        assertThat(loadModificationsApplicationInfos).hasSize(1);

        ModificationApplicationInfos loadModificationApplicationInfos = loadModificationsApplicationInfos.getFirst();
        assertThat(searchResults.getFirst().getModificationUuid()).isEqualTo(loadModificationApplicationInfos.getModificationUuid());

        // search using an equipment identifier that doesn't exist ("notFound")
        hits = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "notFound");
        Assertions.assertThat(hits).isEmpty();

        // search using an equipment identifier on non-existing network
        hits = networkModificationService.searchNetworkModifications(UUID.randomUUID(), "Id");
        assertThat(hits).isEmpty();
    }

    @Test
    void testSearchReferenceModifications() {
        String rootNetworkTag = "PH1";

        // Save a shared modification
        CompositeModificationInfos compositeModificationInfos = CompositeModificationInfos.builder()
            .uuid(UUID.randomUUID())
            .modificationsInfos(List.of(createLoadCreationInfos("newLoadId")))
            .build();
        UUID sharedUuid = modificationRepository.saveModifications(group2Uuid, List.of(ModificationEntity.fromDTO(compositeModificationInfos))).getFirst().getUuid();

        // Save a modification reference
        ModificationReferenceInfos modificationReferenceInfos = ModificationReferenceInfos.builder()
            .referencedId(sharedUuid)
            .referenceType(ModificationReferenceInfos.Type.COMPOSITE)
            .build();
        List<ModificationInfos> modifications = modificationRepository.saveModifications(groupUuid, List.of(ModificationEntity.fromDTO(modificationReferenceInfos)));
        networkModificationRepository.updateRootNetworkApplicability(modifications.stream().map(ModificationInfos::getUuid).toList(), rootNetworkTag, true);

        // Apply modification reference
        modifications = networkModificationRepository.getActiveModifications(groupUuid, rootNetworkTag);
        NetworkModificationResult result = TestUtils.applyModificationsBlocking(networkModificationApplicator,
            TestUtils.groupOnAnyRootNetwork(groupUuid, modifications, reportInfos),
            networkInfos
        );
        assertThat(result).isNotNull();

        List<ModificationApplicationInfos> modificationApplicationInfos = IterableUtils.toList(modificationApplicationInfosRepository.findAll());
        assertThat(modificationApplicationInfos).hasSize(1);

        // Search for equipmentIds containing 'load'
        Map<UUID, List<ModificationsSearchResult>> hits = networkModificationService.searchNetworkModifications(networkInfos.getNetworkUuuid(), "load");
        assertThat(hits).hasSize(1);
        assertNotNull(hits.get(groupUuid));
        List<ModificationsSearchResult> searchResults = hits.get(groupUuid);
        assertThat(searchResults).hasSize(1);
        assertThat(searchResults.getFirst().getImpactedEquipmentIds()).contains("newLoadId");
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
