/*
  Copyright (c) 2026, RTE (http://www.rte-france.com)
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server.modifications;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.powsybl.commons.PowsyblException;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.ModificationType;
import org.gridsuite.modification.dto.AttributeModification;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.GeneratorModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.dto.OperationType;
import org.gridsuite.modification.dto.tabular.TabularModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.gridsuite.modification.server.utils.elasticsearch.DisableElasticsearch;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;

import java.util.List;
import java.util.UUID;
import java.util.stream.Stream;

import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.asyncDispatch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.request;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@Tag("IntegrationTest")
@SpringBootTest
@DisableElasticsearch
@AutoConfigureMockMvc
class ParameterizedNetworkModificationControllerTest {
    private static final UUID TEST_NETWORK_ID = UUID.randomUUID();
    private static final UUID NOT_FOUND_NETWORK_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();

    private static final String URI_NETWORK_MODIFICATION_BASE = "/v1/network-modifications";
    private static final String URI_NETWORK_MODIFICATION_WITH_GROUP = URI_NETWORK_MODIFICATION_BASE + "?groupUuid=" + TEST_GROUP_ID;
    private static final String URI_NETWORK_MODIFICATION_BY_UUID = URI_NETWORK_MODIFICATION_BASE + "/";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper mapper;

    @Autowired
    private NetworkModificationRepository networkModificationRepository;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    @MockitoBean
    private ReportService reportService;

    private Network network;

    @BeforeEach
    void setUp() {
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        networkModificationRepository.deleteAll();

        when(networkStoreService.getNetwork(eq(NOT_FOUND_NETWORK_ID), any(PreloadingStrategy.class))).thenThrow(new PowsyblException());
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), any(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);
    }

    @AfterEach
    void tearDown() {
        networkModificationRepository.deleteAll();
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCreateReadUpdateDeleteModificationThroughServerApi(String caseName,
                                                                  ModificationInfos modificationToCreate,
                                                                  ModificationInfos modificationToUpdate) throws Exception {
        UUID createdModificationUuid = createModification(modificationToCreate);

        assertGroupModificationCount(1);

        ModificationInfos createdModification = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).getFirst();
        assertThat(createdModification).recursivelyEquals(modificationToCreate);

        MvcResult readResult = mockMvc.perform(get(URI_NETWORK_MODIFICATION_BY_UUID + createdModificationUuid))
            .andExpect(status().isOk())
            .andReturn();
        ModificationInfos readModification = mapper.readValue(readResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(readModification).recursivelyEquals(modificationToCreate);

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BY_UUID + createdModificationUuid)
                .content(mapper.writeValueAsString(modificationToUpdate))
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());

        ModificationInfos updatedModification = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).getFirst();
        assertThat(updatedModification).recursivelyEquals(modificationToUpdate);

        mockMvc.perform(delete(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", createdModificationUuid.toString()))
            .andExpect(status().isOk());

        assertTrue(networkModificationRepository.getModifications(TEST_GROUP_ID, false, true).isEmpty());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCreateDisabledModificationWithoutApplyingIt(String caseName,
                                                           ModificationInfos modificationToCreate,
                                                           ModificationInfos modificationToUpdate) throws Exception {
        modificationToCreate.setActivated(false);

        createModification(modificationToCreate);

        List<ModificationInfos> completeModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, completeModifications.size());
        assertThat(completeModifications.getFirst()).recursivelyEquals(modificationToCreate);

        List<ModificationInfos> metadataModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(1, metadataModifications.size());
        assertEquals(false, metadataModifications.getFirst().getActivated());
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldCopyModificationThroughServerApi(String caseName,
                                                ModificationInfos modificationToCreate,
                                                ModificationInfos modificationToUpdate) throws Exception {
        UUID modificationUuid = saveModification(modificationToCreate);

        String body = TestUtils.getJsonBody(List.of(modificationUuid), TEST_NETWORK_ID, null);

        ResultActions copyAction = mockMvc.perform(put("/v1/groups/{groupUuid}?action=COPY", TEST_GROUP_ID)
                .content(body)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());

        mockMvc.perform(asyncDispatch(copyAction.andReturn()))
            .andExpect(status().isOk());

        List<ModificationInfos> copiedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(2, copiedModifications.size());
        assertThat(copiedModifications.get(0)).recursivelyEquals(modificationToCreate);
        assertThat(copiedModifications.get(1)).recursivelyEquals(modificationToCreate);
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("serverModificationCases")
    void shouldStashAndUnstashModificationThroughServerApi(String caseName,
                                                           ModificationInfos modificationToCreate,
                                                           ModificationInfos modificationToUpdate) throws Exception {
        UUID modificationUuid = saveModification(modificationToCreate);

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", modificationUuid.toString())
                .queryParam("stashed", "true"))
            .andExpect(status().isOk());

        List<ModificationInfos> stashedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true, true);
        assertEquals(1, stashedModifications.size());
        assertEquals(Boolean.TRUE, stashedModifications.getFirst().getStashed());

        mockMvc.perform(put(URI_NETWORK_MODIFICATION_BASE)
                .queryParam("groupUuid", TEST_GROUP_ID.toString())
                .queryParam("uuids", modificationUuid.toString())
                .queryParam("stashed", "false"))
            .andExpect(status().isOk());

        List<ModificationInfos> restoredModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, restoredModifications.size());
        assertFalse(Boolean.TRUE.equals(restoredModifications.getFirst().getStashed()));
    }

    private UUID createModification(ModificationInfos modificationToCreate) throws Exception {
        String bodyJson = TestUtils.getJsonBody(modificationToCreate, TEST_NETWORK_ID, null);

        ResultActions createAction = mockMvc.perform(post(URI_NETWORK_MODIFICATION_WITH_GROUP)
                .content(bodyJson)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(request().asyncStarted());

        MvcResult createResult = mockMvc.perform(asyncDispatch(createAction.andReturn()))
            .andExpect(status().isOk())
            .andReturn();

        NetworkModificationsResult networkModificationsResult = mapper.readValue(createResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isPresent());
        assertNotEquals(
            NetworkModificationResult.ApplicationStatus.WITH_ERRORS,
            networkModificationsResult.modificationResults().getFirst().get().getApplicationStatus()
        );

        List<ModificationInfos> storedModifications = networkModificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(1, storedModifications.size());
        return storedModifications.getFirst().getUuid();
    }

    private UUID saveModification(ModificationInfos modificationInfos) {
        ModificationEntity entity = ModificationEntity.fromDTO(modificationInfos);
        networkModificationRepository.saveModifications(TEST_GROUP_ID, List.of(entity));
        return entity.getId();
    }

    private void assertGroupModificationCount(int expectedSize) throws Exception {
        MvcResult result = mockMvc.perform(get("/v1/groups/{groupUuid}/network-modifications?onlyMetadata=true", TEST_GROUP_ID)
                .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();

        List<ModificationInfos> modifications = mapper.readValue(result.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(expectedSize, modifications.size());
    }

    private static Stream<Arguments> serverModificationCases() {
        return Stream.of(
            Arguments.of(
                "equipment attribute modification",
                switchOpenModification(true),
                switchOpenModification(false)
            ),
            Arguments.of(
                "generator modification",
                generatorModification("idGenerator", "generator name from create"),
                generatorModification("idGenerator", "generator name from update")
            ),
            Arguments.of(
                "composite modification",
                compositeModification("composite", "generator name from composite"),
                compositeModification("composite", "generator name from composite")
            ),
            Arguments.of(
                "tabular modification",
                tabularModification("generator name from tabular create"),
                tabularModification("generator name from tabular update")
            )
        );
    }

    private static EquipmentAttributeModificationInfos switchOpenModification(boolean open) {
        return EquipmentAttributeModificationInfos.builder()
            .equipmentId("v1b1")
            .equipmentType(IdentifiableType.SWITCH)
            .equipmentAttributeName("open")
            .equipmentAttributeValue(open)
            .stashed(false)
            .build();
    }

    private static GeneratorModificationInfos generatorModification(String generatorId, String generatorName) {
        return GeneratorModificationInfos.builder()
            .equipmentId(generatorId)
            .equipmentName(new AttributeModification<>(generatorName, OperationType.SET))
            .stashed(false)
            .build();
    }

    private static CompositeModificationInfos compositeModification(String name, String generatorName) {
        return CompositeModificationInfos.builder()
            .name(name)
            .modificationsInfos(List.of(generatorModification("idGenerator", generatorName)))
            .stashed(false)
            .build();
    }

    private static TabularModificationInfos tabularModification(String generatorName) {
        return TabularModificationInfos.builder()
            .modificationType(ModificationType.GENERATOR_MODIFICATION)
            .modifications(List.of(generatorModification("idGenerator", generatorName)))
            .stashed(false)
            .build();
    }
}
