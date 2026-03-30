/**
 * Copyright (c) 2026, RTE (http://www.rte-france.com)
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package org.gridsuite.modification.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.powsybl.iidm.network.IdentifiableType;
import com.powsybl.iidm.network.Network;
import com.powsybl.network.store.client.NetworkStoreService;
import com.powsybl.network.store.client.PreloadingStrategy;
import org.gridsuite.modification.NetworkModificationException;
import org.gridsuite.modification.dto.CompositeModificationInfos;
import org.gridsuite.modification.dto.EquipmentAttributeModificationInfos;
import org.gridsuite.modification.dto.ModificationInfos;
import org.gridsuite.modification.server.dto.NetworkModificationResult;
import org.gridsuite.modification.server.dto.NetworkModificationsResult;
import org.gridsuite.modification.server.entities.ModificationEntity;
import org.gridsuite.modification.server.repositories.NetworkModificationRepository;
import org.gridsuite.modification.server.service.ReportService;
import org.gridsuite.modification.server.utils.NetworkCreation;
import org.gridsuite.modification.server.utils.TestUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.util.Pair;
import org.springframework.http.MediaType;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import static org.gridsuite.modification.ModificationType.COMPOSITE_MODIFICATION;
import static org.gridsuite.modification.server.utils.NetworkCreation.VARIANT_ID;
import static org.gridsuite.modification.server.utils.TestUtils.runRequestAsync;
import static org.gridsuite.modification.server.utils.assertions.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * @author Mathieu Deharbe <mathieu.deharbe at rte-france.com>
 */
@AutoConfigureMockMvc
@SpringBootTest
class CompositeControllerTest {
    private static final UUID TEST_NETWORK_ID = UUID.fromString("7928181c-7977-4592-ba19-88027e4254e4");
    private static final UUID TEST_GROUP_ID = UUID.randomUUID();
    private static final UUID TEST_GROUP2_ID = UUID.randomUUID();
    private static final String URI_COMPOSITE_NETWORK_MODIF_BASE = "/v1/network-composite-modifications";
    private static final String URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT = "/v1/network-composite-modifications/";
    private static final String URI_NETWORK_MODIF_BASE = "/v1/network-modifications";

    @Autowired
    private MockMvc mockMvc;

    private ObjectWriter objectWriter;

    @Autowired
    private NetworkModificationRepository modificationRepository;

    @MockitoBean
    private ReportService reportService;

    @Autowired
    private ObjectMapper mapper;

    @MockitoBean
    private NetworkStoreService networkStoreService;

    private Network network;

    @BeforeEach
    void setUp() {
        objectWriter = mapper.writer().withDefaultPrettyPrinter();
        network = NetworkCreation.create(TEST_NETWORK_ID, true);
        when(networkStoreService.getNetwork(eq(TEST_NETWORK_ID), nullable(PreloadingStrategy.class))).then((Answer<Network>) invocation -> network);
        modificationRepository.deleteAll();
    }

    @AfterEach
    void tearOff() {
        // clean DB
        modificationRepository.deleteAll();
    }

    @Test
    void testSplit() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 2;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // Create a composite modification with the switch modification
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        ModificationInfos compositeModificationInfos = CompositeModificationInfos.builder()
                .modifications(modificationList)
                .build();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(modificationRepository.getModificationInfo(compositeModificationUuid)).recursivelyEquals(compositeModificationInfos);

        List<ModificationInfos> modificationInfosList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(modificationsNumber, modificationInfosList.size());

        // get the composite modification (metadata only)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        Map<UUID, List<ModificationInfos>> compositeModificationsMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<ModificationInfos> compositeModificationContent = compositeModificationsMap.get(compositeModificationUuid);
        assertEquals(modificationsNumber, compositeModificationContent.size());
        for (int i = 0; i < modificationUuids.size(); i++) {
            assertEquals(modificationInfosList.get(i).getMessageValues(), compositeModificationContent.get(i).getMessageValues());
        }
        assertNotNull(compositeModificationContent.getFirst().getMessageType());
        assertNotNull(compositeModificationContent.getFirst().getMessageValues());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeName());
        assertNull(((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeValue());

        // create another composite modification
        List<ModificationInfos> otherModificationList = createSomeSwitchModifications(TEST_GROUP2_ID, modificationsNumber);
        List<UUID> otherModificationUuids = otherModificationList.stream().map(ModificationInfos::getUuid).toList();
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(otherModificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        ModificationInfos otherCompositeModificationInfos = CompositeModificationInfos.builder()
                .modifications(otherModificationList)
                .build();
        UUID otherCompositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertThat(modificationRepository.getModificationInfo(otherCompositeModificationUuid)).recursivelyEquals(otherCompositeModificationInfos);

        // get both composite modifications
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids=" + compositeModificationUuid + "&uuids=" + otherCompositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        Map<UUID, List<ModificationInfos>> bothCompositesMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(2, bothCompositesMap.size());
        assertEquals(modificationsNumber, bothCompositesMap.get(compositeModificationUuid).size());
        assertEquals(modificationsNumber, bothCompositesMap.get(otherCompositeModificationUuid).size());

        // get the composite modification (complete data)
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        Map<UUID, List<ModificationInfos>> completeMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        compositeModificationContent = completeMap.get(compositeModificationUuid);
        checkCompositeModificationContent(compositeModificationContent);

        // Insert the composite modification in the group
        final String bodyJson = getJsonBodyModificationCompositeInfos(
                List.of(Pair.of(compositeModificationUuid, "random name")));
        mvcResult = runRequestAsync(
                mockMvc,
                put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/" + TEST_GROUP_ID + "?action=SPLIT")
                        .content(bodyJson)
                        .contentType(MediaType.APPLICATION_JSON),
                status().isOk());

        assertApplicationStatusOK(mvcResult);

        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber * 2, newModificationList.size());
        List<UUID> newModificationUuidList = newModificationList.stream().map(ModificationInfos::getUuid).toList();
        assertEquals(modificationUuids.getFirst(), newModificationUuidList.getFirst());
        assertThat(modificationList.getFirst()).recursivelyEquals(newModificationList.get(modificationsNumber));
    }

    @Test
    void testInsert() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 2;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);

        // Create a composite modification with the switch modification
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        List<ModificationInfos> modificationInfosList = modificationRepository.getModifications(TEST_GROUP_ID, true, true);
        assertEquals(modificationsNumber, modificationInfosList.size());

        // Insert the composite modification in the group
        final String bodyJson = getJsonBodyModificationCompositeInfos(
                List.of(Pair.of(compositeModificationUuid, "random name")));

        // insert the same composite modification inside as a complete composite, not split into regular network modifications
        mvcResult = runRequestAsync(
                mockMvc,
                put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/" + TEST_GROUP_ID + "?action=INSERT")
                        .content(bodyJson).contentType(MediaType.APPLICATION_JSON), status().isOk()
        );
        assertApplicationStatusOK(mvcResult);
        List<ModificationInfos> newModificationList = modificationRepository.getModifications(TEST_GROUP_ID, false, true);
        assertEquals(modificationsNumber + 1, newModificationList.size());
        CompositeModificationInfos insertedComposite = (CompositeModificationInfos) newModificationList.stream().filter(modificationInfos ->
                modificationInfos.getType().equals(COMPOSITE_MODIFICATION)).findFirst().orElseThrow();
        assertNotNull(insertedComposite);
        checkCompositeModificationContent(insertedComposite.getModifications());
    }

    private static void checkCompositeModificationContent(List<ModificationInfos> compositeModificationContent) {
        assertEquals("open", ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeName());
        assertEquals(Boolean.TRUE, ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentAttributeValue());
        assertEquals(IdentifiableType.SWITCH, ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentType());
        assertEquals("v1b1", ((EquipmentAttributeModificationInfos) compositeModificationContent.getFirst()).getEquipmentId());
    }

    private List<ModificationInfos> createSomeSwitchModifications(UUID groupId, int number) throws Exception {
        List<Boolean> openStates = List.of(true, false);
        EquipmentAttributeModificationInfos switchStatusModificationInfos = EquipmentAttributeModificationInfos.builder()
                .equipmentType(IdentifiableType.SWITCH)
                .equipmentAttributeName("open")
                .equipmentId("v1b1")
                .build();
        MvcResult mvcResult;
        for (int i = 0; i < number; i++) {
            switchStatusModificationInfos.setEquipmentAttributeValue(openStates.get(i % 2));
            String bodyJson = TestUtils.getJsonBody(switchStatusModificationInfos, TEST_NETWORK_ID, VARIANT_ID);
            mvcResult = runRequestAsync(mockMvc, post(URI_NETWORK_MODIF_BASE + "?groupUuid=" + groupId).content(bodyJson).contentType(MediaType.APPLICATION_JSON), status().isOk());
            assertApplicationStatusOK(mvcResult);
        }
        var modificationList = modificationRepository.getModifications(groupId, false, true);
        assertEquals(number, modificationList.size());
        return modificationList;
    }

    private String getJsonBodyModificationCompositeInfos(List<Pair<UUID, String>> modifs) throws JsonProcessingException {
        return TestUtils.getJsonBodyModificationCompositeInfos(modifs, TEST_NETWORK_ID, VARIANT_ID);
    }

    private void assertApplicationStatusOK(MvcResult mvcResult) throws Exception {
        NetworkModificationsResult networkModificationsResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, networkModificationsResult.modificationResults().size());
        assertTrue(networkModificationsResult.modificationResults().getFirst().isPresent());
        assertNotEquals(NetworkModificationResult.ApplicationStatus.WITH_ERRORS, networkModificationsResult.modificationResults().getFirst().get().getApplicationStatus());
    }

    @Test
    void testDuplicateCompositeModification() throws Exception {
        // Create a composite modification with the modification
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 1);
        List<UUID> modificationUuidList = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult;
        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuidList)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Duplicate it without group ownership
        mvcResult = mockMvc.perform(
                        post(URI_COMPOSITE_NETWORK_MODIF_BASE + "/duplication")
                                .content(objectWriter.writeValueAsString(List.of(compositeModificationUuid)))
                                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();

        Map<UUID, UUID> returnedMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(1, returnedMap.size());
        Map.Entry<UUID, UUID> returnedIds = returnedMap.entrySet().stream().findFirst().get();
        UUID returnedSourceId = returnedIds.getKey();
        UUID returnedNewId = returnedIds.getValue();
        assertNotEquals(returnedSourceId, returnedNewId);
        assertEquals(compositeModificationUuid, returnedSourceId);

        ModificationInfos sourceModificationInfos = modificationRepository.getModificationInfo(compositeModificationUuid);
        ModificationInfos newModificationInfos = modificationRepository.getModificationInfo(returnedNewId);
        // compare duplicate with the source (same data except uuid)
        assertThat(sourceModificationInfos).recursivelyEquals(newModificationInfos);
        // source group has not changed
        List<ModificationInfos> groupModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, groupModifications.size());
        assertEquals(modificationUuidList.getFirst(), groupModifications.getFirst().getUuid());

        // now delete the duplicate modification
        mockMvc.perform(delete(URI_NETWORK_MODIF_BASE)
                        .queryParam("uuids", returnedNewId.toString()))
                .andExpect(status().isOk());

        // source group has not changed
        groupModifications = modificationRepository.getModifications(TEST_GROUP_ID, true, true, false);
        assertEquals(1, groupModifications.size());
        assertEquals(modificationUuidList.getFirst(), groupModifications.getFirst().getUuid());
        // duplicate has been deleted
        assertEquals("MODIFICATION_NOT_FOUND : " + returnedNewId, assertThrows(NetworkModificationException.class, ()
                -> modificationRepository.getModificationInfo(returnedNewId)).getMessage());
    }

    @Test
    void testUpdateNetworkCompositeModification() throws Exception {
        // Insert some switch modifications in the group
        int modificationsNumber = 3;
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, modificationsNumber);
        assertEquals(modificationsNumber, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // Create a composite modification with the switch modifications
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Create new modifications to use in the update
        int newModificationsNumber = 2;
        List<ModificationInfos> newModificationList = createSomeSwitchModifications(TEST_GROUP2_ID, newModificationsNumber);
        List<UUID> newModificationUuids = newModificationList.stream().map(ModificationInfos::getUuid).toList();

        // Update the composite modification with the new modifications
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + compositeModificationUuid)
                        .content(mapper.writeValueAsString(newModificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // Get the composite modification content and verify it has been updated
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        Map<UUID, List<ModificationInfos>> updatedMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<ModificationInfos> updatedCompositeContent = updatedMap.get(compositeModificationUuid);

        assertEquals(newModificationsNumber, updatedCompositeContent.size());
    }

    @Test
    void testUpdateNetworkCompositeModificationWithNonexistentUuid() throws Exception {
        // Try to update a composite modification that doesn't exist
        UUID nonExistentUuid = UUID.randomUUID();
        List<UUID> modificationUuids = List.of(UUID.randomUUID());

        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + nonExistentUuid)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    void testUpdateNetworkCompositeModificationWithEmptyList() throws Exception {
        // Create a composite modification with some modifications
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 2);
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();

        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeModificationUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Update the composite with an empty list of modifications
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/" + compositeModificationUuid)
                        .content(mapper.writeValueAsString(Collections.emptyList())).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());

        // Verify that the composite now contains no modifications
        mvcResult = mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}&onlyMetadata=false", compositeModificationUuid))
                .andExpect(status().isOk()).andReturn();
        Map<UUID, List<ModificationInfos>> emptyMap = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        List<ModificationInfos> updatedCompositeContent = emptyMap.get(compositeModificationUuid);

        assertTrue(updatedCompositeContent.isEmpty());
    }

    @Test
    void testMoveSubModificationWithinComposite() throws Exception {
        // Create 3 switch modifications and a composite containing them
        List<ModificationInfos> modificationList = createSomeSwitchModifications(TEST_GROUP_ID, 3);
        List<UUID> modificationUuids = modificationList.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(modificationUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Fetch the actual sub-modification UUIDs as stored inside the composite
        Map<UUID, List<ModificationInfos>> initialMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> subUuids = initialMap.get(compositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(3, subUuids.size());

        // Move the first sub-modification to the end (no beforeUuid = append)
        // was [0,1,2] → [1,2,0]
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/{groupUuid}/sub-modifications/{modificationUuid}",
                        TEST_GROUP_ID, subUuids.get(0))
                        .queryParam("sourceCompositeUuid", compositeUuid.toString())
                        .queryParam("targetCompositeUuid", compositeUuid.toString()))
                .andExpect(status().isOk());

        Map<UUID, List<ModificationInfos>> afterFirstMove = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> orderAfterFirst = afterFirstMove.get(compositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(3, orderAfterFirst.size());
        assertEquals(subUuids.get(1), orderAfterFirst.get(0));
        assertEquals(subUuids.get(2), orderAfterFirst.get(1));
        assertEquals(subUuids.get(0), orderAfterFirst.get(2));

        // Move the last sub-modification before the first using beforeUuid
        // current [1,2,0] → move 0 before 1 → [0,1,2]
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/{groupUuid}/sub-modifications/{modificationUuid}",
                        TEST_GROUP_ID, orderAfterFirst.get(2))
                        .queryParam("sourceCompositeUuid", compositeUuid.toString())
                        .queryParam("targetCompositeUuid", compositeUuid.toString())
                        .queryParam("beforeUuid", orderAfterFirst.get(0).toString()))
                .andExpect(status().isOk());

        Map<UUID, List<ModificationInfos>> afterSecondMove = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> orderAfterSecond = afterSecondMove.get(compositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(3, orderAfterSecond.size());
        assertEquals(subUuids.get(0), orderAfterSecond.get(0));
        assertEquals(subUuids.get(1), orderAfterSecond.get(1));
        assertEquals(subUuids.get(2), orderAfterSecond.get(2));
    }

    @Test
    void testMoveSubModificationFromCompositeToRoot() throws Exception {
        // Create a composite with 2 sub-modifications and add it to the group
        List<ModificationInfos> subMods = createSomeSwitchModifications(TEST_GROUP2_ID, 2);
        List<UUID> subModUuids = subMods.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(subModUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        runRequestAsync(mockMvc,
                put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/{groupUuid}?action=INSERT", TEST_GROUP_ID)
                        .content(getJsonBodyModificationCompositeInfos(List.of(Pair.of(compositeUuid, "composite"))))
                        .contentType(MediaType.APPLICATION_JSON),
                status().isOk());

        // Fetch the actual sub-modification UUIDs as stored inside the composite
        Map<UUID, List<ModificationInfos>> initialMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> actualSubUuids = initialMap.get(compositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(2, actualSubUuids.size());

        int rootSizeBefore = modificationRepository.getModifications(TEST_GROUP_ID, true, true).size();

        // Move first sub-modification from composite to root level (no targetCompositeUuid)
        UUID movingUuid = actualSubUuids.get(0);
        mockMvc.perform(put(URI_COMPOSITE_NETWORK_MODIF_BASE + "/groups/{groupUuid}/sub-modifications/{modificationUuid}",
                        TEST_GROUP_ID, movingUuid)
                        .queryParam("sourceCompositeUuid", compositeUuid.toString()))
                .andExpect(status().isOk());

        // Composite should now contain only 1 sub-modification
        Map<UUID, List<ModificationInfos>> resultMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        assertEquals(1, resultMap.get(compositeUuid).size());
        assertEquals(actualSubUuids.get(1), resultMap.get(compositeUuid).get(0).getUuid());

        // Root group should have one more modification
        assertEquals(rootSizeBefore + 1, modificationRepository.getModifications(TEST_GROUP_ID, true, true).size());

        // The moved modification must now belong to TEST_GROUP_ID (has a group at root level)
        ModificationEntity movedEntity = modificationRepository.getModificationEntity(movingUuid);
        assertNotNull(movedEntity.getGroup());
        assertEquals(TEST_GROUP_ID, movedEntity.getGroup().getId());

        // The remaining sub-modification must still have no group (still owned by the composite)
        ModificationEntity remainingEntity = modificationRepository.getModificationEntity(actualSubUuids.get(1));
        assertNull(remainingEntity.getGroup());
    }

    @Test
    void testExpandToLeafUuids() throws Exception {
        // Create leaf modifications inside a composite
        List<ModificationInfos> leafMods = createSomeSwitchModifications(TEST_GROUP_ID, 3);
        List<UUID> leafUuids = leafMods.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(leafUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID compositeUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Fetch actual sub-modification UUIDs stored inside the composite
        Map<UUID, List<ModificationInfos>> initialMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", compositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> actualSubUuids = initialMap.get(compositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(3, actualSubUuids.size());

        // Expanding the composite UUID should return itself and all sub-modification UUIDs
        mvcResult = mockMvc.perform(get(URI_COMPOSITE_NETWORK_MODIF_BASE + "/leaf-uuids")
                        .queryParam("uuids", compositeUuid.toString()))
                .andExpect(status().isOk()).andReturn();
        Set<UUID> result = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        assertTrue(result.contains(compositeUuid));
        assertTrue(result.containsAll(actualSubUuids));
        assertEquals(4, result.size());

        // Expanding plain leaf UUIDs directly should return them as-is
        mvcResult = mockMvc.perform(get(URI_COMPOSITE_NETWORK_MODIF_BASE + "/leaf-uuids")
                        .queryParam("uuids", actualSubUuids.get(0).toString())
                        .queryParam("uuids", actualSubUuids.get(1).toString()))
                .andExpect(status().isOk()).andReturn();
        Set<UUID> leafResult = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });
        assertEquals(Set.of(actualSubUuids.get(0), actualSubUuids.get(1)), leafResult);
    }

    @Test
    void testExpandToLeafUuidsNestedComposites() throws Exception {
        // Build nested structure: outerComposite → [innerComposite → [leaf1, leaf2], leaf3]
        List<ModificationInfos> innerLeafs = createSomeSwitchModifications(TEST_GROUP_ID, 2);
        List<UUID> innerLeafUuids = innerLeafs.stream().map(ModificationInfos::getUuid).toList();
        MvcResult mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(innerLeafUuids)).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID innerCompositeUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Fetch actual sub-UUIDs of the inner composite
        Map<UUID, List<ModificationInfos>> innerMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", innerCompositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> actualInnerSubUuids = innerMap.get(innerCompositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(2, actualInnerSubUuids.size());

        List<ModificationInfos> outerLeafs = createSomeSwitchModifications(TEST_GROUP2_ID, 1);
        UUID leaf3RootUuid = outerLeafs.get(0).getUuid();

        mvcResult = mockMvc.perform(post(URI_COMPOSITE_NETWORK_MODIF_BASE)
                        .content(mapper.writeValueAsString(List.of(innerCompositeUuid, leaf3RootUuid))).contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk()).andReturn();
        UUID outerCompositeUuid = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        // Fetch actual sub-UUIDs of the outer composite (includes innerComposite and leaf3 copies)
        Map<UUID, List<ModificationInfos>> outerMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", outerCompositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<ModificationInfos> outerSubMods = outerMap.get(outerCompositeUuid);
        assertEquals(2, outerSubMods.size());

        // Identify the inner composite copy and the leaf copy by their type
        UUID actualInnerCompositeUuid = outerSubMods.stream()
                .filter(m -> COMPOSITE_MODIFICATION == m.getType())
                .map(ModificationInfos::getUuid)
                .findFirst().orElseThrow();
        UUID actualLeaf3Uuid = outerSubMods.stream()
                .filter(m -> COMPOSITE_MODIFICATION != m.getType())
                .map(ModificationInfos::getUuid)
                .findFirst().orElseThrow();

        // Fetch actual sub-UUIDs of the inner composite as nested under the outer composite
        Map<UUID, List<ModificationInfos>> nestedInnerMap = mapper.readValue(
                mockMvc.perform(get(URI_GET_COMPOSITE_NETWORK_MODIF_CONTENT + "/network-modifications?uuids={id}", actualInnerCompositeUuid))
                        .andExpect(status().isOk()).andReturn().getResponse().getContentAsString(),
                new TypeReference<>() { });
        List<UUID> actualNestedInnerSubUuids = nestedInnerMap.get(actualInnerCompositeUuid).stream().map(ModificationInfos::getUuid).toList();
        assertEquals(2, actualNestedInnerSubUuids.size());

        // Expanding the outer composite should recursively include all nested UUIDs
        mvcResult = mockMvc.perform(get(URI_COMPOSITE_NETWORK_MODIF_BASE + "/leaf-uuids")
                        .queryParam("uuids", outerCompositeUuid.toString()))
                .andExpect(status().isOk()).andReturn();
        Set<UUID> result = mapper.readValue(mvcResult.getResponse().getContentAsString(), new TypeReference<>() { });

        assertTrue(result.contains(outerCompositeUuid));
        assertTrue(result.contains(actualInnerCompositeUuid));
        assertTrue(result.containsAll(actualNestedInnerSubUuids));
        assertTrue(result.contains(actualLeaf3Uuid));
        assertEquals(5, result.size());
    }
}